{-# LANGUAGE BangPatterns, DeriveGeneric, RecordWildCards #-}
module ReactiveDaemon.OpenZWave where

import           Control.Applicative
-- import Control.Lens
import           Control.Monad
-- import Data.Time.Clock.System
-- import Data.Time.Format
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Word
import           Foreign.C.Types         hiding ( CBool )
import           Foreign.Hoppy.Runtime
import           Foreign.Marshal.Alloc          ( alloca )
import qualified Foreign.Ptr                   as Ptr
import           GHC.Generics
import qualified OpenZWave.Ozw                 as Z
import           OpenZWave.Std
-- import Reactive.Banana.Frameworks

data ZWaveOptions = ZWaveOptions { _dbPath,_cfgPath,_flags
                                 , _driverPath --TODO: probably shouldn't go here
                                     :: String }

--makePrisms ''Z.NotificationType

type HomeId = Word32
type NodeId = CUChar
type ValueId = CULLong

data NodeInfo = NodeInfo { _nodeHome :: !HomeId
                         , _nodeId :: !NodeId
                         , _nodeName :: !Text
                         , _nodeManufacturer :: !Text
                         , _nodeProductName :: !Text
                         , _nodeProductType :: !Text
                         }
                deriving (Show)
-- makeLenses ''NodeInfo

data ValueData = VTBool !Bool
               | VTByte !CUChar
               | VTDecimal !String
               | VTInt !Int
               | VTList !Int ![(Int, String)]
               | VTSchedule
               | VTShort !CShort
               | VTString !String
               | VTButton
               | VTRaw -- ByteString
    deriving (Show, Eq, Generic)
-- makePrisms ''ValueData

data ValueInfo = ValueInfo { _vInfoId   :: !ValueId
                           , _vInfoName :: !String
                           , _vInfoNode :: !NodeId
                           }
    deriving (Show)
-- makeLenses ''ValueInfo


data ZVID = ZVID !HomeId !CULLong
  deriving (Show)

data Notification
  = DriverReady !HomeId
  | NodeAdded !NodeInfo
  | NodeRemoved !HomeId !NodeId
  | ValueAdded !ZVID !ValueInfo !ValueData
  | ValueRemoved !HomeId !NodeId !ValueId
  | ValueChanged !HomeId !NodeId !ValueId !ValueData
  | AwakeNodesQueried
  | AllNodesQueried
  | Unsupported !Z.NotificationType
  deriving (Show)

type NotificationListener = Notification -> IO ()

registerNotificationEvent :: Z.Manager -> NotificationListener -> IO (IO ())
registerNotificationEvent m = addHandler
  where
    addHandler :: NotificationListener -> IO (IO ())
    addHandler l = do
        let notifCb = const . (convertNotification >=> l)
        cb <- Z.haskellOnNotificationCallback_newFunPtr notifCb
        unlessM (Z.manager_AddWatcher m cb Ptr.nullPtr)
            $ fail "could not add notification listener"
        return $ unlessM (Z.manager_RemoveWatcher m cb Ptr.nullPtr) $ putStrLn
            "could not remove notification listener"

    convertNotification :: Z.NotificationConst -> IO Notification
    convertNotification !n = do
        notifType <- Z.notification_GetType n
        -- t <- systemToUTCTime <$> getSystemTime
        -- let timestamp = formatTime defaultTimeLocale
        --                            (dateTimeFmt defaultTimeLocale)
        --                            t
        -- putStrLn $ timestamp ++ ": " ++ show notifType
        case notifType of
            Z.NotificationType_DriverReady ->
                DriverReady <$> Z.notification_GetHomeId n
            Z.NotificationType_NodeAdded -> do
                !hid  <- Z.notification_GetHomeId n
                !nid  <- Z.notification_GetNodeId n
                !name <- Text.pack <$> Z.manager_GetNodeName m hid nid
                !manu <-
                    Text.pack <$> Z.manager_GetNodeManufacturerName m hid nid
                !prod  <- Text.pack <$> Z.manager_GetNodeProductName m hid nid
                !pType <- Text.pack <$> Z.manager_GetNodeProductType m hid nid
                return . NodeAdded $ NodeInfo hid nid name manu prod pType
            Z.NotificationType_NodeRemoved ->
                NodeRemoved
                    <$> Z.notification_GetHomeId n
                    <*> Z.notification_GetNodeId n
            Z.NotificationType_ValueAdded -> do
                v' <- Z.notification_GetValueID n
                v  <- ZVID <$> Z.notification_GetHomeId n <*> Z.valueID_GetId v'
                ValueAdded v <$> extractValue v' <*> convertValue m v'
            Z.NotificationType_ValueRemoved -> do
                valueId <- Z.notification_GetValueID n
                ValueRemoved
                    <$> Z.notification_GetHomeId n
                    <*> Z.valueID_GetNodeId valueId
                    <*> Z.valueID_GetId valueId
            Z.NotificationType_ValueChanged -> do
                !v     <- Z.notification_GetValueID n
                !name  <- Z.manager_GetValueLabel m v
                !data' <- convertValue m v
                !nid   <- Z.valueID_GetNodeId v
                !vid   <- Z.valueID_GetId v
                !hid   <- Z.notification_GetHomeId n
                -- TODO: Printing here forces these thunks to be evaluated.
                --       When this is removed, shit breaks...
                --       Test again without bang patters in the do above do block
                putStrLn $ "  " ++ show (hid, nid, vid, name, data')
                return $ ValueChanged hid nid vid data'
            Z.NotificationType_AwakeNodesQueried -> return AwakeNodesQueried
            Z.NotificationType_AllNodesQueried -> return AllNodesQueried
            t -> return $ Unsupported t

    extractValue v =
        ValueInfo
            <$> Z.valueID_GetId v
            <*> Z.manager_GetValueLabel m v
            <*> Z.valueID_GetNodeId v

defaultOptions :: ZWaveOptions
defaultOptions = ZWaveOptions "/usr/local/etc/openzwave"
                              "/home/stephen/.openzwave"
                              "--ConsoleOutput false"
                              "/dev/ttyACM0" -- "/dev/cu.usbmodem1421"

initOzw :: ZWaveOptions -> IO Z.Manager
initOzw ZWaveOptions {..} = do
    o <- Z.options_Create _dbPath _cfgPath _flags
    unlessM (Z.options_Lock o) $ fail "could not lock options"
    zwManager <- Z.manager_Create
    unlessM (Z.manager_AddDriver zwManager _driverPath)
        $ fail "could not add driver"
    return zwManager

setValue :: Z.Manager -> ZVID -> ValueData -> IO Bool
setValue m (ZVID !hid !vid) d = do
    v <- toGc =<< Z.valueID_unpack hid vid
    --name <- Z.manager_GetValueLabel m v
    -- t <- Z.valueID_GetType v
    --putStrLn $ " " ++ show (name, vid, t) ++ " <- " ++ show d
    case d of
        (VTByte   b) -> Z.manager_setByteValue m v b
        (VTBool   b) -> Z.manager_setBoolValue m v b
        -- (VTDecimal d ) -> error "todo: implement setValue for decimal"
        (VTInt    i) -> Z.manager_setIntValue m v i
        -- (VTList idx _) -> Z.manager_SetValueListSelection m v idx
        (VTShort  s) -> Z.manager_setShortValue m v s
        (VTString s) -> Z.manager_SetStringValue m v s
        _            -> error "todo: implement setValue"

setValueFromString :: Z.Manager -> ZVID -> String -> IO Bool
setValueFromString m (ZVID !hid !vid) d = do
    v <- toGc =<< Z.valueID_unpack hid vid
    Z.manager_SetStringValue m v d

convertValue :: Z.Manager -> Z.ValueIDConst -> IO ValueData
convertValue mgr = go
  where
    go v = do
        t <- Z.valueID_GetType v
        case t of
            Z.ValueType_Bool -> alloca $ \p -> do
                b <- Z.manager_GetValueAsBool mgr v p
                unless b $ fail
                    "value reported as bool but could not be converted"
                VTBool <$> decode p
            Z.ValueType_Byte -> alloca $ \p -> do
                b <- Z.manager_GetValueAsByte mgr v p
                unless b $ fail
                    "value reported as byte but could not be converted"
                w <- decode p
                return $ VTByte w
            Z.ValueType_Decimal -> do
                p <- toGc =<< stdString_new
                b <- Z.manager_GetValueAsString mgr v p
                unless b $ fail
                    "value reported as decimal but could not be converted"
                VTDecimal <$> decode p
            Z.ValueType_Int -> alloca $ \p -> do
                b <- Z.manager_GetValueAsInt mgr v p
                unless b $ fail
                    "value reported as int but could not be converted"
                VTInt <$> decode p
            Z.ValueType_List -> do
                labelsP <- toGc =<< Z.stringVector_new
                valuesP <- toGc =<< Z.intVector_new
                unlessM
                        (liftA2 (&&)
                                (Z.manager_GetValueListItems mgr v labelsP)
                                (Z.manager_GetValueListValues mgr v valuesP)
                        )
                    $ fail "value reported as list but could not be converted"
                l <- alloca $ \p -> do
                    unlessM (Z.manager_GetValueListSelection mgr v p)
                        $ fail "could not get list selection"
                    decode p
                VTList l
                    <$> (zip <$> (toContents valuesP) <*> (toContents labelsP))
            Z.ValueType_Schedule -> return VTSchedule
            Z.ValueType_Short    -> alloca $ \p -> do
                b <- Z.manager_GetValueAsShort mgr v p
                unless b $ fail
                    "value reported as short but could not be converted"
                v' <- decode p
                return $ VTShort v'
            Z.ValueType_String -> do
                p <- toGc =<< stdString_new
                b <- Z.manager_GetValueAsString mgr v p
                unless b $ fail
                    "value reported as string but could not be converted"
                VTString <$> decode p
            Z.ValueType_Button -> return VTButton
            Z.ValueType_Raw    -> return $ VTRaw --(error "implement VTRaw")

unlessM, whenM :: Monad m => m Bool -> m () -> m ()
unlessM mbool action = mbool >>= flip unless action
whenM mbool action = mbool >>= flip when action
