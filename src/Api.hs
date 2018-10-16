{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Conduit

import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Monad.Reader

import qualified Data.Aeson                    as JSON
import           Data.Aeson.Types               ( )
import           Data.ByteString.Lazy           ( fromStrict )
import           Data.ByteString.Lazy.Builder   ( toLazyByteString )
import           Data.ByteString.Lazy.Char8     ( unpack )
import qualified Data.Conduit.List             as CL
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text 
import           Data.Word

import           Foreign.C.Types

import           GHC.Generics

import qualified Network.WebSockets            as WS

import qualified ReactiveDaemon.OpenZWave as Z

import           Servant.API
import           Servant.API.WebSocketConduit
import           Servant.Client
import           Servant.Client.Core

type HomeId = Integer
type DeviceId = Integer
type ValueId = Integer

-- TODO: re-implement (see below)
data ValueState = VBool Bool
                | VByte Integer
                | VDecimal Text
                | VInt Int
                | VList Int [(Int, String)]
                | VSchedule
                | VShort Integer
                | VString Text
                | VButton
                | VRaw
  deriving (Show, Generic)

instance JSON.ToJSON ValueState
instance JSON.FromJSON ValueState

convertZWaveValue :: Z.ValueData -> ValueState
convertZWaveValue (Z.VTBool b) = VBool b
convertZWaveValue (Z.VTByte c) = VByte $ toInteger c
convertZWaveValue (Z.VTDecimal s) = VDecimal $ Text.pack s
convertZWaveValue (Z.VTInt i) = VInt i
convertZWaveValue (Z.VTList i l) = VList i l
convertZWaveValue Z.VTSchedule = VSchedule
convertZWaveValue (Z.VTShort s) = VShort $ toInteger s
convertZWaveValue (Z.VTString s) = VString $ Text.pack s
convertZWaveValue Z.VTButton = VButton
convertZWaveValue Z.VTRaw = VRaw

convertToZWaveValue :: ValueState -> Z.ValueData
convertToZWaveValue (VBool b) = Z.VTBool b
convertToZWaveValue (VByte i) = Z.VTByte $ fromInteger i
convertToZWaveValue (VDecimal t) = Z.VTDecimal $ Text.unpack t
convertToZWaveValue (VInt i) = Z.VTInt i
convertToZWaveValue (VList i l) = Z.VTList i l
convertToZWaveValue VSchedule = Z.VTSchedule
convertToZWaveValue (VShort i) = Z.VTShort $ fromInteger i
convertToZWaveValue (VString t) = Z.VTString $ Text.unpack t
convertToZWaveValue VButton = Z.VTButton
convertToZWaveValue VRaw = Z.VTRaw

data Value = Value { _valueId :: ValueId
                   , _valueState :: ValueState
                   }
  deriving (Show, Generic)
instance JSON.ToJSON Value
instance JSON.FromJSON Value
type ValueMap = Map ValueId Value

data Device = Device { _deviceId :: DeviceId
                     , _deviceValues :: ValueMap
                     }
  deriving (Show, Generic)
instance JSON.ToJSON Device
instance JSON.FromJSON Device
type DeviceMap = Map DeviceId Device

data Home = Home { _homeId :: HomeId
                 , _homeDevices :: DeviceMap
                 }
  deriving (Show, Generic)
instance JSON.ToJSON Home
instance JSON.FromJSON Home
type HomeMap = Map HomeId Home

type API =
  "state" :> WebSocketConduit () HomeMap
  :<|> Capture "home" HomeId
    :> Capture "device" DeviceId
    :> Capture "value" ValueId
    :> (ReqBody '[JSON] ValueState :> Post '[JSON] Value
      :<|> Get '[JSON] Value)

--TODO: improve nested API
--  https://haskell-servant.readthedocs.io/en/stable/tutorial/Server.html#nested-apis

type ConduitClient i o = Conduit i (ResourceT IO) o

class (RunClient m) => RunWebSocketClient m where
  webSocketRequest
    :: (JSON.ToJSON o, JSON.FromJSON i)
    => ConduitClient i o -> Request -> m ()

instance RunWebSocketClient ClientM where
  webSocketRequest client req = do
    ClientEnv m burl cookieJar <- ask
    let (host, port, path) = toClientInfo burl req
    liftIO . WS.runClient host port path $
      \conn -> do
        WS.forkPingThread conn 10
        i <- newEmptyMVar
        race_ (forever $ WS.receiveData conn >>= putMVar i) $ do
          runConduitRes $
              forever (yieldM . liftIO $ takeMVar i)
                  .| CL.mapMaybe (JSON.decode . fromStrict)
                  .| client
                  .| CL.mapM_
                      (liftIO . WS.sendTextData conn . JSON.encode)
          WS.sendClose conn ("Out of data" :: Text)
          -- drop all new packets until connection actually closes
          -- (exception thrown)
          forever $ WS.receiveDataMessage conn

toClientInfo :: BaseUrl -> Request -> (String, Int, String)
toClientInfo burl req = (host, port, path)
 where
  host = baseUrlHost burl
  port = baseUrlPort burl
  path = baseUrlPath burl <> (unpack . toLazyByteString $ requestPath req)

instance (RunWebSocketClient m, JSON.FromJSON o, JSON.ToJSON i) =>
    HasClient m (WebSocketConduit i o)
  where
    type Client m (WebSocketConduit i o) = ConduitClient o i -> m ()
    clientWithRoute _ _ req client = webSocketRequest client req
    hoistClientMonad _ _ f client c = f (client c)

----- ARCHIVED -----
{- 
data LiftWS a = LiftWS a
  deriving (Typeable, Eq, Show, Functor, Traversable, Foldable, Bounded)

----------- New Runner Monad

type WsResponse = () --TODO 

class (RunWebSocketClient m) => HasWebSocketClient m api where
  type WsClient (m :: * -> *) (api :: *) :: *
  wsClientWithRoute
    :: Proxy m -> Proxy api -> Request -> WsClient m api

instance (RunWebSocketClient m, FromJSON o, ToJSON i)
    => HasWebSocketClient m (WebSocketConduit i o)
  where
    type WsClient m (WebSocketConduit i o) = ConduitClient o i -> m ()
    wsClientWithRoute _ _ = flip wsRequest

----------- New Client Monad

newtype WsClientM a = WsClientM { runWsClient :: ClientM a }
  deriving (Functor, Applicative, Monad, MonadReader ClientEnv, MonadIO)

instance RunClient WsClientM where
  streamingRequest = WsClientM . streamingRequest
  throwServantError = WsClientM  . throwServantError
  runRequest = WsClientM . runRequest

instance RunWebSocketClient WsClientM where
  wsRequest client req = do
    ClientEnv m burl cookieJar <- ask
    let (host, port, path) = toClientInfo burl req
    liftIO . WS.runClient host port path $
      \conn -> do
        WS.forkPingThread conn 10
        i <- newEmptyMVar
        race_ (forever $ WS.receiveData conn >>= putMVar i) $ do
          runConduitRes $ forever (yieldM . liftIO $ takeMVar i)
                      .| CL.mapMaybe (decode . fromStrict)
                      .| client
                      .| CL.mapM_ (liftIO . WS.sendTextData conn . encode)
          WS.sendClose conn ("Out of data" :: Text)
          -- drop all new packets until connection actually closes
          -- (exception thrown)
          forever $ WS.receiveDataMessage conn

instance ClientLike (WsClientM a) (WsClientM a) where
  mkClient = id
-}
