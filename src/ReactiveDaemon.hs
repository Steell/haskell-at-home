{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module ReactiveDaemon where

import Control.Applicative
import Control.Arrow
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
-- import qualified Data.Time.Clock as Clock
import Foreign.C.Types hiding (CBool)
import qualified OpenZWave.Ozw as Z
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.Environment (getArgs)
import Text.PrettyPrint

-- Daemons
-- import Control.Pipe.Serialize (serializer, deserializer)
-- import Pipes ( runEffect, (<-<), await, yield )
-- import Control.Pipe.Socket (Handler)
-- import Control.Concurrent.TVar (TVar, newTVar, modifyTVar)
-- import Network.Socket ( withSocketsDo )
-- import System.Daemon

-- Local
import ReactiveDaemon.OpenZWave

----

data Cmd = ListDevices
         | FixNetwork
         -- | SetValue ZNode ZValue ZValueData
         | InitOzw
makePrisms ''Cmd

data Scene = DoubleDown | TripleDown | DoubleUp | TripleUp
  deriving (Eq)
makePrisms ''Scene

type Value'' = ((ValueData -> IO Bool), ValueInfo, ValueData)
type ValueMap = Map ValueId Value''
type DeviceMap = Map NodeId ValueMap

data ZWaveDevice = ZWaveDevice { _zwdValues  :: Behavior ValueMap
                               , _zwdChanges :: Event (ValueId, ValueData)
                               }
makeLenses ''ZWaveDevice
data ZWaveValue = ZWaveValue { _zwvInfo    :: Behavior (Maybe Value'')
                             , _zwvChanges :: Event ValueData
                             }
makeLenses ''ZWaveValue

data ZWData = ZWData { _zwdMap     :: Behavior DeviceMap
                     , _zwdChanged :: Event (NodeId, ValueId, ValueData)
		     -- , _zwdCurrentTime :: Behavior Clock.UTCTime
                     }
makeClassy ''ZWData

newtype ZWave m a = ZWave { unzwio :: ReaderT ZWData m a }
    deriving (Functor, Applicative, Monad, MonadReader ZWData, MonadTrans)

data ZWaveEvent = DeviceList DeviceMap

----

{-

dMain2 :: IO ()
dMain2 = withSocketsDo $ do
  registryVar <- newTVarIO Map.empty
  let options = def {daemonPort = 7857} -- TODO: configurable via cmd args
  ensureDaemonWithHandlerRunning "OZWd" options (handleCommands registryVar)
  void $ runClient "localhost" (daemonPort options) InitOzw

data NetworkState = NetworkState { homeId :: HomeId
                                 , devices :: DeviceMap
                                 }
data Registry = Registry { ozwManager :: STM Z.Manager
                         , broadcastChan :: STM (TChan NetworkState)
                         }

handleCommands :: TMVar Registry -> Handler ()
handleCommands registryVar reader writer = runEffect $
    writer <-< serializer <-< commandExecuter <-< deserializer <-< reader
  where
    commandExecutor = forever $ do
      comm <- await
      case comm of
        ListDevices -> do
          (c, r) <- atomically $ do
            r <- readTMVar registryVar
            c <- broadcastChan r
            return (c, r)
          let stateReader = forever $ do
                ds <- atomically $ readTChan c
                yield (DeviceList ds)
          forkIO . runEffect $ writer <-< serializer <-< stateReader
          
        FixNetwork -> do
          (mgr, NetworkState{..}) <- atomically $ do
            r <- readTMVar registryVar
            m <- ozwManager r
            c <- broadcastChan r
            ns <- readTChan c
            return (m, ns)
          Z.manager_HealNetwork mgr homeId True
          yield OK
          
        SetValue n v x -> do
          (mgr, NetworkState{..}) <- atomically $ do
            r <- readTMVar registryVar
            m <- ozwManager r
            c <- broadcastChan r
            ns <- readTChan c
            return (m, ns)
          case Map.lookup v (Map.lookup n devices) of
            Nothing -> yield Failed
            Just (setter, _, _) -> do
              success <- setter x
              yield $ if success then OK else Failed

        InitOzw -> join . atomically $ do
          r <- tryReadTMVar registryVar
          case r of
            Just _ -> return $ yield OK
            Nothing -> do
              mgrVar <- newEmptyTMVar
              bChan <- newTChan
              writeTChan bChan Map.empty
              putTMVar registryVar $ Registry (readTMVar mgrVar) (cloneTChan bChan)
              return $ do
                mgr <- initOzw defaultOptions
                ozwLoop mgr bChan
                atomically $ putTMVar mgrVar mgr
                yield OK

ozwLoop :: Z.Manager -> TChan NetworkState -> IO ()
ozwLoop mgr c = void $ registerNotificationEvent mgr handler
  where
    handler (DriverReady h) = atomically $ do
      ns <- readTChan c
      writeTChan c $ ns { homeId = h }

    handler (ValueChanged (nid, vid, d)) = atomically $ do
      ns <- readTChan c
      writeTChan c $
        ns & devices %~ Map.adjust (Map.adjust (_3 .~ d) vid) nid

    handler (ValueAdded (vid, i@ValueInfo{..}, d)) = atomically $ do
      ns <- readTChan c
      writeTChan c $
        ns & devices %~
          Map.insertWith Map.union
                         _vInfoNode
                         (Map.singleton _vInfoId (setValue mgr vid, i, d))

    handler (ValueRemoved (nid, vid)) = atomically $ do
      ns <- readTChan c
      writeTChan c $
        ns & devices %~
          Map.update (partial (not . Map.null) . Map.delete vid) nid

-}

dNetwork :: Z.Manager
         -> (String -> IO ())
         -> AddHandler Cmd
         -> ZWave Moment (Event (IO ()))
         -> MomentIO ()
dNetwork mgr write cmdHandler cfg = do
    -- inputs
    eCmd <- fromAddHandler cmdHandler
    eNotif <- fromAddHandler . AddHandler $ registerNotificationEvent mgr

    -- graph
    let eDriverReady = filterJust $ (preview _DriverReady) <$> eNotif
    bHomeId <- stepper Nothing (Just <$> eDriverReady)
    let eValueChanged = filterJust $ (preview _ValueChanged) <$> eNotif
        eValueInfoAdded = filterJust $ (preview _ValueAdded) <$> eNotif
        eValueRemoved = filterJust $ (preview _ValueRemoved) <$> eNotif
    --eNodeInfoAdded = filterJust $ (preview _NodeAdded) <$> eNotif
    --eNodeRemoved = filterJust $ (preview _NodeRemoved) <$> eNotif
    bDeviceMap <- valueMapB mgr eValueInfoAdded eValueChanged eValueRemoved
    let eAllNodesQueried = filterJust $ (preview _AwakeNodesQueried) <$> eNotif

    -- currentTimeB <- fromPoll Clock.getCurrentTime

    eCfg <- liftMoment . flip runReaderT
                              (ZWData bDeviceMap
                                      eValueChanged
                                      -- currentTimeB
                              ) $
                unzwio cfg
    activeCfg <- switchE $ eAllNodesQueried $> eCfg
    reactimate activeCfg

    let eListDevices = filterJust $ (preview _ListDevices) <$> eCmd
    	eFixNetwork = filterJust $ (preview _FixNetwork) <$> eCmd

    reactimate $ (bDeviceMap <&> (printMap >>> show >>> write)) <@ eListDevices
    reactimate $
        (bHomeId <&> flip whenJust (\h -> Z.manager_HealNetwork mgr h True)) <@ eFixNetwork

valueMapB :: MonadMoment m
          => Z.Manager
          -> Event (ZVID, ValueInfo, ValueData)
          -> Event (NodeId, ValueId, ValueData)
          -> Event (NodeId, ValueId)
          -> m (Behavior DeviceMap)
valueMapB mgr eAdded eChanged eRemoved =
    accumB Map.empty $
        unions [ addValue <$> eAdded
               , updateValue <$> eChanged
               , removeValue <$> eRemoved
               ]
  where
    addValue (vid, i@ValueInfo{..}, d) =
        Map.insertWith Map.union
                       _vInfoNode
                       (Map.singleton _vInfoId (setValue mgr vid, i, d))
    updateValue (_vInfoNode, _vInfoId, d) =
        Map.adjust (Map.adjust (_3 .~ d) _vInfoId) _vInfoNode
    removeValue (nid, vid) =
        Map.update (partial (not . Map.null) . Map.delete vid) nid

dMain :: ZWave Moment (Event (IO ())) -> IO ()
dMain cfg = do
    [device] <- getArgs
  
    outputChan <- newChan
    (inputHandler, write) <- newAddHandler

    void . forkIO $ do
        mgr <- initOzw defaultOptions { _driverPath = device }
        let nd = dNetwork mgr (writeChan outputChan) inputHandler cfg
        network <- compile nd
        actuate network

    let writeOut = readChan outputChan >>= putStrLn . ("> " ++)
    void . forkIO $ forever writeOut

    let repl = getLine >>=
            \case
                "devices" -> do
                    write ListDevices
                    repl
		"fix" -> do
		    write FixNetwork
		    repl
                "q" -> return ()
                _ -> repl
    repl

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (pure ()) f mg

printMap :: DeviceMap -> Doc
printMap map = Map.toList map & fmap toDoc & vcat
  where
    toDoc (nid, vMap) = text (show nid) $+$ nest 2 (Map.toList vMap & fmap vToDoc & vcat)
    vToDoc (_, (_, ValueInfo{..}, vdata)) = text (_vInfoName ++ ": " ++ show vdata)

($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

partial :: (a -> Bool) -> a -> Maybe a
partial p x = if p x then Just x else Nothing

-----------------

--TODO: probably gonna want two monads: ZWaveConfig and ZWaveAction
--      maybe this lends itself more to Reflex than it does reactive-banana?

-- currentTimeB :: Monad m => ZWave m (Behavior Clock.UTCTime)
-- currentTimeB = view zwdCurrentTime

getValue :: ZWaveValue -> Behavior (Maybe ValueData)
getValue ZWaveValue{..} = fmap (fmap (view _3)) _zwvInfo

setValueByteOnEvt :: ZWaveValue -> Event CUChar -> Event (IO ())
setValueByteOnEvt v e = setValueByte' v <@> e

setValuesByte :: [ZWaveValue] -> CUChar -> Moment (IO ())
setValuesByte vs b = fmap (void . sequence) . sequence $
    fmap (flip setValueByte b) vs

setValueByte :: ZWaveValue -> CUChar -> Moment (IO ())
setValueByte v b = fmap ($ b) $ valueB (setValueByte' v)

setValueByte' :: ZWaveValue -> Behavior (CUChar -> IO ())
setValueByte' ZWaveValue{..} =
    _zwvInfo <&>
        (fmap (view $ _1 . to wrap) >>> fromMaybe (const $ pure ()))
  where
    wrap setter = void . setter . VTByte

getDeviceById :: Monad m => NodeId -> ZWave m ZWaveDevice
getDeviceById nid = do
    bDeviceMap <- view zwdMap
    eValueChanged <- view zwdChanged
    return ZWaveDevice { _zwdValues = bDeviceMap <&>
                           (Map.lookup nid >>> fromMaybe Map.empty)
                       , _zwdChanges = fmap splice $
                           filterE (has $ _1 . only nid) eValueChanged
                       }
  where
    splice (_, a, b) = (a, b)

getDeviceValueByName :: String -> ZWaveDevice -> ZWaveValue
getDeviceValueByName name ZWaveDevice{..} =
    let b' :: Behavior (Maybe (ValueId, Value''))
        b' = _zwdValues <&>
            ((Map.toList >>>
                  List.find (has $ _2 . _2 . vInfoName . only name)))
        blookup :: Behavior ((ValueId, ValueData) -> Maybe ValueData)
        blookup = b' <&>
            maybe (const Nothing)
                  (\(vid, _) (vid', vdata) ->
                       if vid == vid' then Just vdata else Nothing)
    in
        ZWaveValue { _zwvInfo = fmap snd <$> b'
                   , _zwvChanges = filterJust $ blookup <@> _zwdChanges
                   }

valueChanges :: ZWaveValue -> Event ValueData
valueChanges ZWaveValue{..} =
    _zwvChanges
