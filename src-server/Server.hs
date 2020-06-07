{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server
  ( serverApp
  , ServerEnv(..)
  , ZWaveState(..) --TODO don't expose this
  , emptyZWaveState
  )
where

import           Api                     hiding ( getValue )

import           Conduit

import           Control.Concurrent.STM         ( atomically )
import           Control.Concurrent.STM.TVar    ( TVar )
import qualified Control.Concurrent.STM.TVar   as TVar
import           Control.Concurrent.STM.TChan   ( TChan )
import qualified Control.Concurrent.STM.TChan  as TChan
import           Control.Monad                  ( unless )
import           Control.Monad.Reader           ( ReaderT
                                                , runReaderT
                                                , ask
                                                )

import qualified Data.Map.Strict               as Map
import           Data.Typeable                  ( Proxy(..) )

import           Network.Wai

-- TODO: this is gross, don't overload alias
import qualified OpenZWave.Ozw                 as Z

import qualified ReactiveDaemon.OpenZWave      as Z

import           Servant

data ZWaveState = ZWaveState { _homeMap :: HomeMap }

emptyZWaveState :: ZWaveState
emptyZWaveState = ZWaveState Map.empty -- AM.empty AM.empty AM.empty

data ServerEnv =
  ServerEnv { _state :: TVar ZWaveState
            , _manager :: Z.Manager
            , _stateBroadcastChan :: TChan HomeMap
            , _eventBroadcastChan :: TChan ZEvent
            }

type AppM = ReaderT ServerEnv Handler

serverApi :: Proxy API
serverApi = Proxy

serverApp :: ServerEnv -> Application
serverApp env = serve serverApi
  $ hoistServer serverApi (toHandler env) (server env)
  where toHandler = flip runReaderT

server :: ServerEnv -> ServerT API AppM
server env =
  stateBroadcast env :<|> eventBroadcast env :<|> values :<|> snapshot :<|> addDevice :<|> cancelAdd
 where
  values hid did vid = postValue hid did vid :<|> getValue hid did vid

postValue :: HomeId -> DeviceId -> ValueId -> SetValue -> AppM ()
postValue hid _ vid newVal = do
  liftIO . putStrLn $ "attempt: " <> show vid <> " <== " <> show newVal
  ServerEnv {..} <- ask
  let setValue (FromString s) m h v = Z.setValueFromString m h v s
      setValue (FromState s) m h v = Z.setValue m h v (convertToZWaveValue s)
  success        <- liftIO $ setValue newVal
                                      _manager
                                      (fromInteger hid)
                                      (fromInteger vid)
  liftIO . putStrLn $ if success then " success!" else " failed!"
  unless success $ throwError err404

getValue :: HomeId -> DeviceId -> ValueId -> AppM Value
getValue hid did vid = do
  homeMap <- snapshot
  let mValue =
        Map.lookup vid
          .   _deviceValues
          =<< Map.lookup did
          .   _homeDevices
          =<< Map.lookup hid homeMap
  case mValue of
    Just value -> return value
    Nothing    -> throwError err404

-- TODO: make own vesion of WebSocketConduit that gives access to AppM
--   possibly Server m (WebSocketConduit i o) = m (Conduit i (ResourceT IO) o)
stateBroadcast :: MonadIO m => ServerEnv -> Conduit () m HomeMap
stateBroadcast ServerEnv {..} = do
  (chan, h0) <- liftIO . atomically $ do
    c     <- TChan.dupTChan _stateBroadcastChan
    hmap0 <- _homeMap <$> TVar.readTVar _state
    return (c, hmap0)
  yield h0
  go chan
 where
  go chan = do
    hmap <- liftIO . atomically $ TChan.readTChan chan
    yield hmap
    go chan

eventBroadcast :: MonadIO m => ServerEnv -> Conduit () m ZEvent
eventBroadcast ServerEnv {..} = do
  (chan, h0) <- liftIO . atomically $ do
    c     <- TChan.dupTChan _eventBroadcastChan
    hmap0 <- _homeMap <$> TVar.readTVar _state
    return (c, hmap0)
  yield $ Init h0
  go chan
 where
  go chan = do
    event <- liftIO . atomically $ TChan.readTChan chan
    yield event
    go chan

snapshot :: AppM HomeMap
snapshot = do
  ServerEnv {..} <- ask
  fmap _homeMap . liftIO . atomically $ TVar.readTVar _state

addDevice :: HomeId -> Bool -> AppM ()
addDevice hid secure = liftIO . void $ Z.addNode hid secure

cancelAdd :: HomeId -> AppM ()
cancelAdd hid = liftIO . void $ Z.cancelControllerCommand hid
