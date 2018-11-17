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

import           Api

import           ClientApi

import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader

import           Data.Foldable                  ( find )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
-- import qualified Data.Time.Clock as Clock

import           Reactive.Banana
import           Reactive.Banana.Frameworks

import           Servant.Client                 ( Client )

----

data Scene = DoubleDown | TripleDown | DoubleUp | TripleUp
  deriving (Eq)
makePrisms ''Scene

data DeviceInfo = DeviceInfo { _dInfo :: Device
                             , _dHomeId :: HomeId
                             }
makeLenses ''DeviceInfo
data ZWaveDevice = ZWaveDevice { _zwdInfo    :: Behavior (Maybe DeviceInfo)
                               , _zwdChanges :: Event ValueMap
                               }
makeLenses ''ZWaveDevice
data ValueInfo = ValueInfo { _vInfo :: Value
                           , _vHomeId :: HomeId
                           , _vDeviceId :: DeviceId
                           }
makeLenses ''ValueInfo
data ZWaveValue = ZWaveValue { _zwvInfo    :: Behavior (Maybe ValueInfo)
                             , _zwvChanges :: Event ValueState
                             }
makeLenses ''ZWaveValue

type HomeInfo = Home
data ZWaveHome = ZWaveHome { _zwhInfo :: Behavior (Maybe HomeInfo)
                           , _zwhChanges :: Event DeviceMap
                           }
makeLenses ''ZWaveHome

data ZWData = ZWData { _zwMap     :: Behavior HomeMap
                     , _zwChanges :: Event (Map HomeId HomeInfo)
                     , _zwSetValue   :: HomeId -> DeviceId -> ValueId -> ValueState -> IO ()
                     -- , _zwdCurrentTime :: Behavior Clock.UTCTime
                     }
makeClassy ''ZWData

newtype ZWave m a = ZWave { unzwio :: ReaderT ZWData m a }
    deriving (Functor, Applicative, Monad, MonadReader ZWData, MonadTrans, MonadFix)

instance MonadMoment m => MonadMoment (ZWave m) where
  liftMoment = ZWave . lift . liftMoment

dNetwork
  :: Client IO API
  -> ZWave Moment (Event (IO ())) --TODO: just use MomentIO and let user cfg have access to reactimate?
  -> AddHandler HomeMap
  -> MomentIO ()
dNetwork client cfg evtHandler = do
  eState <- fromAddHandler evtHandler
  zd     <- zwData client eState
  eCfg   <- liftMoment . flip runReaderT zd $ unzwio cfg
  reactimate eCfg

zwData :: MonadMoment m => Client IO API -> Event HomeMap -> m ZWData
zwData client eState = do
  let setValue' = setValue client
  bHomeMap <- stepper Map.empty eState
  return $ ZWData bHomeMap eState setValue'

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (pure ()) f mg

($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

partial :: (a -> Bool) -> a -> Maybe a
partial p x = if p x then Just x else Nothing

-- currentTimeB :: Monad m => ZWave m (Behavior Clock.UTCTime)
-- currentTimeB = view zwdCurrentTime

getValue :: ZWaveValue -> Behavior (Maybe ValueState)
getValue ZWaveValue {..} = fmap (_valueState . _vInfo) <$> _zwvInfo

setValueByteOnEvt
  :: Monad m => ZWaveValue -> Event Integer -> ZWave m (Event (IO ()))
setValueByteOnEvt v e = do
  bSetter <- setValueByte' v
  return $ bSetter <@> e

setValueByte' :: Monad m => ZWaveValue -> ZWave m (Behavior (Integer -> IO ()))
setValueByte' ZWaveValue {..} = do
  setter <- view zwSetValue
  let getSetter :: Maybe ValueInfo -> Integer -> IO ()
      getSetter Nothing               _ = pure ()
      getSetter (Just ValueInfo {..}) i = do
        putStrLn
          $  "{ node: "
          ++ show _vDeviceId
          ++ ", valueName: \""
          ++ _valueName _vInfo
          ++ "\", oldValue: "
          ++ show (_valueState _vInfo)
          ++ " } <= "
          ++ show i
        void . setter _vHomeId _vDeviceId (_valueId _vInfo) $ VByte i
  return $ getSetter <$> _zwvInfo

getHomeById :: Monad m => HomeId -> ZWave m ZWaveHome
getHomeById h = do
  bHomeMap <- view zwMap
  eChange  <- view zwChanges
  return ZWaveHome
    { _zwhInfo    = Map.lookup h <$> bHomeMap
    , _zwhChanges = filterJust $ fmap _homeDevices . Map.lookup h <$> eChange
    }

getDeviceById :: MonadMoment m => DeviceId -> ZWaveHome -> ZWave m ZWaveDevice
getDeviceById deviceId ZWaveHome {..} = do
  let
    mkInfo Home {..} = do
      d <- Map.lookup deviceId _homeDevices
      return $ DeviceInfo d _homeId

    info :: Behavior (Maybe DeviceInfo)
    info = _zwhInfo <&> (>>= mkInfo)

    changes :: Event ValueMap
    changes =
      filterJust $ _zwhChanges <&> (Map.lookup deviceId >>> fmap _deviceValues)

  --changes' <- {- distinct-} changes --TODO
  return ZWaveDevice {_zwdInfo = info, _zwdChanges = changes}

app :: Monad m => m (a -> m b) -> a -> m b
app mf a = mf >>= ($ a)

getDeviceValueByName :: MonadMoment m => String -> ZWaveDevice -> m ZWaveValue
getDeviceValueByName name ZWaveDevice {..} = do
  let mkValueInfo :: DeviceInfo -> Maybe ValueInfo
      mkValueInfo DeviceInfo {..} = do
        v <- find ((== name) . _valueName) . Map.elems $ _deviceValues _dInfo
        return ValueInfo
          { _vInfo     = v
          , _vDeviceId = _deviceId _dInfo
          , _vHomeId   = _dHomeId
          }

      info :: Behavior (Maybe ValueInfo)
      info = _zwdInfo <&> (>>= mkValueInfo)

      lookup :: ValueInfo -> ValueMap -> Maybe ValueState
      lookup ValueInfo {..} = fmap _valueState . Map.lookup (_valueId _vInfo)

      blookup :: Behavior (ValueMap -> Maybe ValueState)
      blookup = info <&> maybe (const Nothing) lookup

      changes = filterJust $ blookup <@> _zwdChanges

  --changes' <- {- distinct -} changes --TODO
  return ZWaveValue {_zwvInfo = info, _zwvChanges = changes}

distinct :: (MonadMoment m, Eq a) => Event a -> m (Event a)
distinct = fmap (filterJust . fst) . mapAccum Nothing . fmap f
 where
  f y (Just x) = if x == y then (Nothing, Just x) else (Just y, Just y)
  f y Nothing  = (Just y, Just y)

valueEvents :: ZWaveValue -> Event ValueState
valueEvents ZWaveValue {..} = _zwvChanges

-- TODO: remove once server/client arch has been updated to send events 
--       rather than whole state snapshots
valueChanges :: MonadMoment m => ZWaveValue -> m (Event ValueState)
valueChanges = distinct . valueEvents
