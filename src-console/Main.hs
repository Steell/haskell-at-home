{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Api                     hiding ( getValue
                                                , setValue
                                                )

import           Control.Monad.Reader
import           Control.Monad.IO.Class         ( MonadIO )

import           Data.Aeson                     ( FromJSON
                                                , decode
                                                )
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust )
import           Data.Text.Encoding             ( encodeUtf8 )

import           Network.HTTP.Client            ( newManager
                                                , defaultManagerSettings
                                                )

import           Options.Generic --TODO replace with optparse-applicative

import           Servant.Client

data Command = ListHomes
             | ListDevices { homeId :: HomeId }
             | ListValues { homeId :: HomeId, deviceId :: DeviceId }
             | GetValue { homeId :: HomeId, deviceId :: DeviceId, valueId :: ValueId}
             | SetValue { homeId :: HomeId, deviceId :: DeviceId, valueId :: ValueId, value :: Text}
             | HealNetwork
  deriving (Generic, Show)
instance ParseRecord Command

main :: IO ()
main = do
    command <- getRecord "ZWave Controller"
    mgr <- newManager defaultManagerSettings
    let env    = mkClientEnv mgr (BaseUrl Http "localhost" 8081 "")
        action = case command of
            ListHomes                       -> listHomes
            HealNetwork                     -> healNetwork
            ListDevices { homeId }          -> listDevices homeId
            ListValues { homeId, deviceId } -> listValues homeId deviceId
            GetValue { homeId, deviceId, valueId } ->
                getValue homeId deviceId valueId
            SetValue { homeId, deviceId, valueId, value } ->
                setValue homeId deviceId valueId (fromJust $ decodeText value)
    runReaderT action env

infixl 4 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

infixl 4 <$<
(<$<) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
g <$< f = fmap g . f

decodeText :: FromJSON a => Text -> Maybe a
decodeText = decode . BL.fromStrict . encodeUtf8

type MonadAction m = (MonadReader ClientEnv m, MonadIO m)

listHomes :: MonadAction m => m ()
listHomes = do
    env <- ask
    liftIO . withZWaveClient env $ do
        ids <- zGetSnapshot <&> extractHomeList
        liftIO $ print ids
    where extractHomeList = fmap show . Map.keys

healNetwork :: MonadAction m => m ()
healNetwork = error "TODO expose heal api"

listDevices :: MonadAction m => HomeId -> m ()
listDevices homeId = do
    env <- ask
    liftIO
        .   withZWaveClient env
        $   liftIO
        .   print
        .   fmap unlines
        =<< fmap extractDeviceList zGetSnapshot
  where
    extractDeviceList :: HomeMap -> Maybe [String]
    extractDeviceList = fmap showDevice . Map.elems . _homeDevices <$< Map.lookup homeId

    showDevice :: Device -> String
    showDevice Device {_deviceId=did, _deviceName=dName, _deviceManufacturer=dMan, _deviceProductName=dpName, _deviceProductType=dpType} =
        "{ id=" <> show did 
          <> ", name=" <> show dName 
          <> ", manufacturer=" <> show dMan
          <> ", product=" <> show dpName
          <> ", type=" <> show dpType

listValues :: MonadAction m => HomeId -> DeviceId -> m ()
listValues homeId deviceId = do
    env <- ask
    liftIO
        .   withZWaveClient env
        $   liftIO
        .   print
        =<< fmap extractValueList zGetSnapshot
  where
    extractValueList =
        fmap _deviceValues
            .   Map.lookup deviceId
            .   _homeDevices
            <=< Map.lookup homeId

getValue :: MonadAction m => HomeId -> DeviceId -> ValueId -> m ()
getValue homeId deviceId valueId = do
    env <- ask
    liftIO
        .   withZWaveClient env
        $   liftIO
        .   print
        =<< fmap extractValue zGetSnapshot
  where
    extractValue =
        _valueState
            <$< Map.lookup valueId
            .   _deviceValues
            <$< Map.lookup deviceId
            .   _homeDevices
            <$< Map.lookup homeId

setValue
    :: (MonadAction m) => HomeId -> DeviceId -> ValueId -> ValueState -> m ()
setValue homeId deviceId valueId value = do
    env <- ask
    liftIO . withZWaveClient env $ do
        zSetValue homeId deviceId valueId value
        liftIO $ putStrLn "OK!"
