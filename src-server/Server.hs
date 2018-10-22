{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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

import           Api

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
import           Network.Wai.Handler.Warp

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
            }

type AppM = ReaderT ServerEnv Handler

serverApi :: Proxy API
serverApi = Proxy

serverApp :: ServerEnv -> Application
serverApp env = serve serverApi
  $ hoistServer serverApi (toHandler env) (server env)
  where toHandler = flip runReaderT

server :: ServerEnv -> ServerT API AppM
server env = stateBroadcast env :<|> values
  where values hid did vid = postValue hid did vid :<|> getValue hid did vid

postValue :: HomeId -> DeviceId -> ValueId -> ValueState -> AppM ()
postValue hid did vid newVal = do
  ServerEnv {..} <- ask
  success <- liftIO $ Z.setValue _manager
                                 (Z.ZVID (fromInteger hid) (fromInteger vid))
                                 (convertToZWaveValue newVal)
  unless success $ throwError err404

getValue :: HomeId -> DeviceId -> ValueId -> AppM Value
getValue hid did vid = do
  ServerEnv {..} <- ask
  homeMap        <- fmap _homeMap . liftIO . atomically $ TVar.readTVar _state
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