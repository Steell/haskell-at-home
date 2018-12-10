{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import           Data.Proxy                     ( Proxy(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

import           GHC.Generics                   ( Generic )

import qualified Network.WebSockets            as WS

import qualified ReactiveDaemon.OpenZWave      as Z

import           Servant.API
import           Servant.API.WebSocketConduit
import           Servant.Client
import           Servant.Client.Core

type HomeId = Integer
type DeviceId = Integer
type ValueId = Integer

-- TODO: use Z.ValueData and do convertions in [To|From]JSON
--       rather than operate on these in the client
data ValueState = VBool Bool
                | VByte Integer
                | VDecimal Float
                | VInt Int
                | VList Int [(Int, String)]
                | VSchedule
                | VShort Integer
                | VString Text
                | VButton
                | VRaw
  deriving (Eq, Show, Read, Generic)

instance JSON.ToJSON ValueState
instance JSON.FromJSON ValueState

convertZWaveValue :: Z.ValueData -> ValueState
convertZWaveValue (Z.VTBool    b) = VBool b
convertZWaveValue (Z.VTByte    c) = VByte $ toInteger c
convertZWaveValue (Z.VTDecimal f) = VDecimal f
convertZWaveValue (Z.VTInt     i) = VInt i
convertZWaveValue (Z.VTList i l ) = VList i l
convertZWaveValue Z.VTSchedule    = VSchedule
convertZWaveValue (Z.VTShort  s)  = VShort $ toInteger s
convertZWaveValue (Z.VTString s)  = VString $ Text.pack s
convertZWaveValue Z.VTButton      = VButton
convertZWaveValue Z.VTRaw         = VRaw

convertToZWaveValue :: ValueState -> Z.ValueData
convertToZWaveValue (VBool    b) = Z.VTBool b
convertToZWaveValue (VByte    i) = Z.VTByte $ fromInteger i
convertToZWaveValue (VDecimal f) = Z.VTDecimal f
convertToZWaveValue (VInt     i) = Z.VTInt i
convertToZWaveValue (VList i l)  = Z.VTList i l
convertToZWaveValue VSchedule    = Z.VTSchedule
convertToZWaveValue (VShort  i)  = Z.VTShort $ fromInteger i
convertToZWaveValue (VString t)  = Z.VTString $ Text.unpack t
convertToZWaveValue VButton      = Z.VTButton
convertToZWaveValue VRaw         = Z.VTRaw

data Value = Value { _valueId :: ValueId
                   , _valueState :: ValueState
                   , _valueName :: Text
                   }
  deriving (Eq, Show, Generic)
instance JSON.ToJSON Value
instance JSON.FromJSON Value
type ValueMap = Map ValueId Value

data Device = Device { _deviceId :: DeviceId
                     , _deviceName :: Text
                     , _deviceManufacturer :: Text
                     , _deviceProductName :: Text
                     , _deviceProductType :: Text
                     , _deviceValues :: ValueMap
                     }
  deriving (Eq, Show, Generic)
instance JSON.ToJSON Device
instance JSON.FromJSON Device
type DeviceMap = Map DeviceId Device

data Home = Home { _homeId :: HomeId
                 , _homeDevices :: DeviceMap
                 }
  deriving (Eq, Show, Generic)
instance JSON.ToJSON Home
instance JSON.FromJSON Home
type HomeMap = Map HomeId Home

data ZEvent = Init HomeMap
            | HomeAdded Home
            | DeviceAdded HomeId Device
            | DeviceRemoved HomeId DeviceId
            | ValueAdded HomeId DeviceId Value
            | ValueRemoved HomeId DeviceId ValueId
            | ValueChanged HomeId DeviceId ValueId ValueState
  deriving (Show, Generic)

instance JSON.ToJSON ZEvent
instance JSON.FromJSON ZEvent

data SetValue = FromString String
              | FromState ValueState
  deriving (Generic, Show)
instance JSON.ToJSON SetValue
instance JSON.FromJSON SetValue

type API =  "state"  :> WebSocketConduit () HomeMap
       :<|> "events" :> WebSocketConduit () ZEvent
       :<|> Capture "home" HomeId
         :> Capture "device" DeviceId
         :> Capture "value" ValueId
         :> (    ReqBody '[JSON] SetValue :> Post '[JSON] ()
            :<|> Get '[JSON] Value)
       :<|> "snapshot" :> Get '[JSON] HomeMap

--TODO: improve nested API
--  https://haskell-servant.readthedocs.io/en/stable/tutorial/Server.html#nested-apis

type ConduitClient i o = ConduitT i o (ResourceT IO) ()

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

clientIO :: ClientEnv -> Client IO API
clientIO cenv = hoistClient api runClientIO (client api)
 where
  runClientIO :: ClientM a -> IO a
  runClientIO = fmap (either (error . show) id) . flip runClientM cenv
  api :: Proxy API
  api = Proxy

handleState :: Monad m => Client m API -> ConduitClient HomeMap () -> m ()
handleState (f :<|> _) = f

handleEvents :: Monad m => Client m API -> ConduitClient ZEvent () -> m ()
handleEvents (_ :<|> f :<|> _) = f

getSnapshot :: Monad m => Client m API -> m HomeMap
getSnapshot (_ :<|> _ :<|> _ :<|> f) = f

setValueString
  :: Monad m => Client m API -> HomeId -> DeviceId -> ValueId -> String -> m ()
setValueString c h d v = setValue c h d v . FromString

setValueState
  :: Monad m
  => Client m API
  -> HomeId
  -> DeviceId
  -> ValueId
  -> ValueState
  -> m ()
setValueState c h d v = setValue c h d v . FromState

setValue
  :: Monad m
  => Client m API
  -> HomeId
  -> DeviceId
  -> ValueId
  -> SetValue
  -> m ()
setValue (_ :<|> _ :<|> valueApi :<|> _) h d v = fst (valueApi h d v)
  where fst (f :<|> _) = f

getValue :: Monad m => Client m API -> HomeId -> DeviceId -> ValueId -> m Value
getValue (_ :<|> _ :<|> valueApi :<|> _) h d v = snd $ valueApi h d v
  where snd (_ :<|> f) = f


--- Attempt at making this a monad

newtype ClientT m a = ClientT { unClientT :: ReaderT (Client m API) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

class Monad m => MonadZWave m where
  zHandleState :: ConduitClient HomeMap () -> m ()
  zHandleEvents :: ConduitClient ZEvent () -> m ()
  zSetValue :: HomeId -> DeviceId -> ValueId -> SetValue -> m ()
  zGetValue :: HomeId -> DeviceId -> ValueId -> m Value
  zGetSnapshot :: m HomeMap

instance Monad m => MonadZWave (ClientT m) where
  zHandleState cc = ClientT . ReaderT $ \c -> handleState c cc
  zHandleEvents cc = ClientT . ReaderT $ \c -> handleEvents c cc
  zGetValue h d v = ClientT . ReaderT $ \c -> getValue c h d v
  zSetValue h d v s = ClientT . ReaderT $ \c -> setValue c h d v s
  zGetSnapshot = ClientT $ ReaderT getSnapshot

zSetValueString
  :: MonadZWave m => HomeId -> DeviceId -> ValueId -> String -> m ()
zSetValueString h d v = zSetValue h d v . FromString

zSetValueState
  :: MonadZWave m => HomeId -> DeviceId -> ValueId -> ValueState -> m ()
zSetValueState h d v = zSetValue h d v . FromState

withZWaveClient :: ClientEnv -> ClientT IO a -> IO a
withZWaveClient env c = flip runReaderT (clientIO env) $ unClientT c

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
