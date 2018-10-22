{-# language DeriveGeneric, LambdaCase, RecordWildCards #-}
module Daemon where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Pipe.Serialize ( serializer, deserializer )
import Control.Pipe.Socket

import Data.ByteString hiding (putStrLn)
import Data.Int
import qualified Data.List as List
import Data.Serialize hiding (decode)
import Data.Text
import Data.Time.Clock.System
import Data.Time.Format
import Data.Word

import Foreign.C.Types hiding (CBool)
import Foreign.Hoppy.Runtime
import Foreign.Ptr (Ptr)
import qualified Foreign.Ptr as Ptr
import Foreign.Storable

import GHC.Generics

import qualified OpenZWave.Ozw as Z
import OpenZWave.Std

import Pipes

import System.IO.Error

type DeviceId = CUChar
--type Device = (DeviceId, Text)
type HomeId = Word32

data ZWaveNetwork = ZWaveNetwork { _devices :: [DeviceId]
                                 , _values :: [Z.ValueIDConst]
                                 , _homeId :: HomeId
                                 , _manager :: Z.Manager
                                 }
addDevice :: DeviceId -> ZWaveNetwork -> ZWaveNetwork
addDevice d s@ZWaveNetwork{..} =
    s { _devices = d : _devices }

removeDevice :: DeviceId -> ZWaveNetwork -> ZWaveNetwork
removeDevice d s@ZWaveNetwork{..} =
    s { _devices = List.delete d _devices }

addValue :: Z.ValueIDConst -> ZWaveNetwork -> ZWaveNetwork
addValue v s@ZWaveNetwork{..} =
    s { _values = v : _values }

type ValueId = Word64

data DeviceValue = VTBool Bool
                 | VTByte Word8
                 | VTDecimal String
                 | VTInt Int
                 | VTList [String]
                 | VTSchedule
                 | VTShort Int16
                 | VTString String
                 | VTButton
                 | VTRaw -- ByteString
  deriving (Generic, Show)
instance Serialize DeviceValue

getDeviceInfo :: ZWaveNetwork -> DeviceId -> IO DeviceInfo
getDeviceInfo ZWaveNetwork{..} d@(CUChar w) = do
    name <- Z.manager_GetNodeName _manager _homeId d
    values <- getValues
    return DeviceInfo { _deviceId = w
                      , _deviceName = name
                      , _deviceValues = values
                      }
  where
    getValues :: IO [ValueInfo]
    getValues = do
        vis <- filterM isValueOfNode _values
        convertValue _manager `mapM` vis

    isValueOfNode vid = do
        nodeId <- Z.valueID_GetNodeId vid
        return $ nodeId == d

eitherToMaybe (Right x) = Just x
eitherToMaybe _ = Nothing

convertValue :: Z.Manager -> Z.ValueIDConst -> IO ValueInfo
convertValue mgr v = do
    l <- Z.manager_GetValueLabel mgr v
    CULLong id' <- Z.valueID_GetId v
    putStrLn $ "  Value: " ++ l ++ " (" ++ show id' ++ ")"
    CUChar did <- Z.valueID_GetNodeId v
    v' <- eitherToMaybe <$> tryIOError (go v)
    return $ ValueInfo did id' l v'
  where
    go v = do
        t <- Z.valueID_GetType v
        case t of
            Z.ValueType_Bool -> do
                let p = Ptr.nullPtr :: Ptr CBool
                b <- Z.manager_GetValueAsBool mgr v p
                when (not b) $
                    fail "value reported as bool but could not be converted"
                VTBool <$> decode p
            Z.ValueType_Byte -> do
                let p = Ptr.nullPtr :: Ptr CUChar
                b <- Z.manager_GetValueAsByte mgr v p
                when (not b) $
                    fail "value reported as byte but could not be converted"
                CUChar w <- decode p
                return $ VTByte w
            Z.ValueType_Decimal -> do
                let p = nullptr :: StdString
                b <- Z.manager_GetValueAsString mgr v p
                when (not b) $
                    fail "value reported as decimal but could not be converted"
                VTDecimal <$> decode p
            Z.ValueType_Int -> do
                let p = Ptr.nullPtr :: Ptr CInt
                b <- Z.manager_GetValueAsInt mgr v p
                when (not b) $
                    fail "value reported as int but could not be converted"
                VTInt <$> decode p
            Z.ValueType_List -> return $ VTList [] -- (error "implement VTList")
            Z.ValueType_Schedule -> return VTSchedule
            Z.ValueType_Short -> do
                let p = Ptr.nullPtr :: Ptr CShort
                b <- Z.manager_GetValueAsShort mgr v p
                when (not b) $
                    fail "value reported as short but could not be converted"
                CShort v' <- decode p
                return $ VTShort v'
            Z.ValueType_String -> do
                let p = nullptr :: StdString
                b <- Z.manager_GetValueAsString mgr v p
                when (not b) $
                    fail "value reported as string but could not be converted"
                VTString <$> decode p
            Z.ValueType_Button -> return VTButton
            Z.ValueType_Raw -> return $ VTRaw --(error "implement VTRaw")
            _ -> undefined


data ValueInfo = ValueInfo { _valueDeviceId :: Word8
                           , _valueId :: ValueId
                           , _valueName :: String
                           , _value :: Maybe DeviceValue
                           }
    deriving (Generic, Show)
instance Serialize ValueInfo

data DeviceInfo = DeviceInfo { _deviceId :: Word8
                             , _deviceName :: String --TODO: replace with Text, need Serialize instance
                             , _deviceValues :: [ValueInfo]
                             }
    deriving (Generic, Show)
instance Serialize DeviceInfo

data ZWaveEvent = NodeAdded DeviceInfo
                | DeviceList [DeviceInfo] [Maybe ValueInfo]
    deriving (Generic, Show)
instance Serialize ZWaveEvent

data ZWaveCommand = AddDevice | ListDevices
    deriving (Generic)
instance Serialize ZWaveCommand

--convert (CUChar w) = w

whenM mbool action = mbool >>= flip when action
ifM mbool actionT actionF = do
  b <- mbool
  if b then actionT else actionF

initializeOzw :: TChan ZWaveEvent -> TVar (Maybe ZWaveNetwork) -> IO ()
initializeOzw chan state = do

    o <- Z.options_Create ("/usr/local/etc/openzwave" :: String)
                          ("/Users/stephen/.openzwave" :: String)
                          ("--ConsoleOutput false" :: String)
    locked <- Z.options_Lock o
    zwManager <- Z.manager_Create

    let notification n _ = do
            notifType <- Z.notification_GetType n
            t <- systemToUTCTime <$> getSystemTime
            let timestamp = formatTime defaultTimeLocale
                                       (dateTimeFmt defaultTimeLocale)
                                       t
            putStrLn $ timestamp ++ ": " ++ show notifType
            case notifType of
                Z.NotificationType_DriverReady -> do
                    homeId <- Z.notification_GetHomeId n
                    atomically $ do
                        writeTVar state $
                            Just ZWaveNetwork { _devices = []
                                              , _values = []
                                              , _homeId = homeId
                                              , _manager = zwManager
                                              }
                Z.NotificationType_NodeAdded -> do
                    nodeId <- Z.notification_GetNodeId n
{-
                    homeId <- readMVar homeIdRef
                    name <- pack <$> Z.manager_GetNodeName zwManager
                                                           homeId
                                                           nodeId
-}

                    state' <- atomically $ do
                        x <- readTVarMaybe state
                        let x' = addDevice nodeId x
                        writeTVar state $! Just $! x'
                        return x'

                    deviceInfo <- getDeviceInfo state' nodeId
                    atomically $ writeTChan chan $ NodeAdded deviceInfo
                Z.NotificationType_NodeRemoved -> do
                    nodeId <- Z.notification_GetNodeId n
                    atomically $ do
                        modifyTVarMaybe' state (removeDevice nodeId)
                    --cb $ NodeDeleted nodeId
                Z.NotificationType_ValueAdded -> do
                    valueId <- Z.notification_GetValueID n
                    atomically $ modifyTVarMaybe' state (addValue valueId)
                _ -> return ()
    cb <- Z.haskellOnNotificationCallback_newFunPtr notification
    added <- Z.manager_AddWatcher zwManager cb Ptr.nullPtr
    added <- Z.manager_AddDriver zwManager device

    return ()
  where
    device :: String
    device = "/dev/cu.usbmodem1421" --TODO: replace with Reader Config
                                   --"/dev/ttyACM0"

readTVarMaybe :: TVar (Maybe a) -> STM a
readTVarMaybe tv = readTVar tv >>=
    \case
        Nothing -> retry
        Just a -> return a

modifyTVarMaybe' :: TVar (Maybe a) -> (a -> a) -> STM ()
modifyTVarMaybe' tvar f = do
  x <- readTVarMaybe tvar
  writeTVar tvar $! Just $! f x

daemon :: MVar Bool -> TChan ZWaveEvent -> TVar (Maybe ZWaveNetwork) -> Handler ()
daemon init chan state reader writer = do
    initialized <- takeMVar init
    when (not initialized) (void . forkIO $ initializeOzw chan state)
    putMVar init True
    runEffect $
        writer <-< serializer <-< commandExecuter <-< deserializer <-< reader
  where
    commandExecuter = forever $
        await >>=
            \case
                AddDevice -> do
                    r <- lift $ do
                             chan' <- atomically $ dupTChan chan
                             ZWaveNetwork{..} <- atomically $
                                                     readTVarMaybe state
                             success <- Z.manager_AddNode _manager _homeId False

                             let go = do
                                     r <- readTChan chan'
                                     case r of
                                         NodeAdded _ -> return r
                                         _ -> retry
                             atomically go
                    yield r
                ListDevices -> do
                    (devices, values) <- lift $ do
                                             network <- atomically $
                                                            readTVarMaybe state
                                             ds <- getDeviceInfo network `mapM`
                                                       _devices network
                                             --vs <- convertValue (_manager network) `mapM`
                                             --          _values network
                                             return (ds, [])
                    yield $ DeviceList devices values
