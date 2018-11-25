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

import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader

import           Data.Foldable                  ( find )
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as Text

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
  deriving (Show)
makeLenses ''DeviceInfo

data ValueInfo = ValueInfo { _vInfo :: Value
                           , _vDeviceInfo :: DeviceInfo
                           }
  deriving (Show)
makeLenses ''ValueInfo
data ValueEvent = VValueChanged ValueState
data ZWaveValue = ZWaveValue { _zwvInfo    :: Behavior (Maybe ValueInfo)
                             , _zwvChanges :: Event ValueEvent
                             }
makeLenses ''ZWaveValue

data DeviceEvent = DValueAdded ValueInfo
                 | DValueRemoved ValueId
                 | DValueChanged ValueId ValueState
data ZWaveDevice = ZWaveDevice { _zwdInfo    :: Behavior (Maybe DeviceInfo)
                               , _zwdChanges :: Event DeviceEvent
                               }
makeLenses ''ZWaveDevice

type HomeInfo = Home
data HomeEvent = HDeviceAdded DeviceInfo
               | HDeviceRemoved DeviceId
               | HValueAdded DeviceId ValueInfo
               | HValueRemoved DeviceId ValueId
               | HValueChanged DeviceId ValueId ValueState
  deriving (Show)
data ZWaveHome = ZWaveHome { _zwhInfo :: Behavior (Maybe HomeInfo)
                           , _zwhChanges :: Event HomeEvent
                           }
makeLenses ''ZWaveHome

data ZWData = ZWData { _zwMap     :: Behavior HomeMap
                     , _zwChanges :: Event ZEvent
                     , _zwSetValue   :: HomeId -> DeviceId -> ValueId -> ValueState -> IO ()
                     -- , _zwdCurrentTime :: Behavior Clock.UTCTime
                     }
makeClassy ''ZWData

newtype ZWave m a = ZWave { unzwio :: ReaderT ZWData m a }
    deriving (Functor, Applicative, Monad, MonadReader ZWData, MonadTrans, MonadFix)

instance MonadMoment m => MonadMoment (ZWave m) where
  liftMoment = ZWave . lift . liftMoment

data ZEventSource a = ZEventSource { _eventTag :: String
                                   , _eventData :: a
                                   }
  deriving (Show)
makeLenses ''ZEventSource

instance Functor ZEventSource where
  fmap f es = es { _eventData = f (_eventData es) }

instance Applicative ZEventSource where
  pure = ZEventSource ""
  ZEventSource {_eventTag=ta,_eventData=f} <*> ZEventSource {_eventTag=tb,_eventData=b} =
    ZEventSource (ta <> tb) $ f b

instance Foldable ZEventSource where
  foldMap f ZEventSource {..} = f _eventData
  foldr f z ZEventSource {..} = f _eventData z

instance Traversable ZEventSource where
  traverse f ZEventSource {..} = ZEventSource _eventTag <$> f _eventData

dNetwork
  :: Client IO API
  -> ZWave MomentIO ()
  -> AddHandler HomeMap
  -> AddHandler ZEvent
  -> MomentIO ()
dNetwork client cfg stateHandler evtHandler = do
  eState <- fromAddHandler stateHandler
  eEvt   <- fromAddHandler evtHandler
  zd     <- zwData client eState eEvt
  flip runReaderT zd $ unzwio cfg

zwData
  :: MonadMoment m => Client IO API -> Event HomeMap -> Event ZEvent -> m ZWData
zwData client eState events = do
  let setValue' = setValueState client
  bHomeMap <- stepper Map.empty eState
  return $ ZWData bHomeMap events setValue'

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

setValueOnEvent
  :: ZWaveValue -> Event (ZEventSource ValueState) -> ZWave MomentIO ()
setValueOnEvent ZWaveValue {..} events = do
  setter <- view zwSetValue
  let getSetter :: Maybe ValueInfo -> ZEventSource ValueState -> IO ()
      getSetter Nothing _               = pure ()
      getSetter (Just v@ValueInfo {..}) event = do
        log v event
        void $ setter (_dHomeId _vDeviceInfo) (_deviceId $ _dInfo _vDeviceInfo) (_valueId _vInfo) (_eventData event)
  lift $ reactimate $ getSetter <$> _zwvInfo <@> events
 where
  log ValueInfo {..} ZEventSource {..} =
    putStrLn
      $  _eventTag
      <> ": "
      <> show _eventData
      <> " => "
      <> "{ device: "
      <> Text.unpack (_deviceName $ _dInfo _vDeviceInfo)
      <> ", valueName: \""
      <> Text.unpack (_valueName _vInfo)
      <> "\", oldValue: "
      <> show (_valueState _vInfo)
      <> " }"

getHomeById :: MonadMoment m => HomeId -> ZWave m ZWaveHome
getHomeById h = do
  bHomeMap <- view zwMap
  eChange  <- view zwChanges
  let changes = observeE . filterJust $ toHomeEvent bHomeMap <$> eChange
  return ZWaveHome {_zwhInfo = Map.lookup h <$> bHomeMap, _zwhChanges = changes}
 where
  toHomeEvent :: Behavior HomeMap -> ZEvent -> Maybe (Moment HomeEvent)
  toHomeEvent bHomeMap = go
   where
    go (DeviceAdded h' d) | h == h' =
      Just . return . HDeviceAdded $ DeviceInfo d h
    go (DeviceRemoved h' d) | h == h'       = Just . return $ HDeviceRemoved d
    go (ValueAdded h' deviceId v) | h == h' = Just $ do
      Just d <-
        valueB bHomeMap
          <&> (Map.lookup h' >=> _homeDevices >>> Map.lookup deviceId)
      return . HValueAdded deviceId . ValueInfo v $ DeviceInfo d h
    go (ValueRemoved h' d v) | h == h' = Just . return $ HValueRemoved d v
    go (ValueChanged h' d v s) | h == h' = Just . return $ HValueChanged d v s
    go _                               = Nothing

getDeviceById :: MonadMoment m => DeviceId -> ZWaveHome -> ZWave m ZWaveDevice
getDeviceById deviceId ZWaveHome {..} = do
  let mkInfo Home {..} = do
        d <- Map.lookup deviceId _homeDevices
        return $ DeviceInfo d _homeId

      info :: Behavior (Maybe DeviceInfo)
      info = _zwhInfo <&> (>>= mkInfo)

      changes :: Event DeviceEvent
      changes = filterJust $ _zwhChanges <&> toDeviceEvent

  return ZWaveDevice {_zwdInfo = info, _zwdChanges = changes}
 where
  toDeviceEvent :: HomeEvent -> Maybe DeviceEvent
  toDeviceEvent (HValueAdded d v) | d == deviceId = Just $ DValueAdded v
  toDeviceEvent (HValueRemoved d v) | d == deviceId = Just $ DValueRemoved v
  toDeviceEvent (HValueChanged d v s) | d == deviceId = Just $ DValueChanged v s
  toDeviceEvent _ = Nothing

app :: Monad m => m (a -> m b) -> a -> m b
app mf a = mf >>= ($ a)

getDeviceValueByName :: MonadMoment m => String -> ZWaveDevice -> m ZWaveValue
getDeviceValueByName name ZWaveDevice {..} = do
  let
    mkValueInfo :: DeviceInfo -> Maybe ValueInfo
    mkValueInfo d@DeviceInfo {..} = do
      v <-
        find ((== name) . Text.unpack . _valueName) . Map.elems $ _deviceValues
          _dInfo
      return ValueInfo {_vInfo = v, _vDeviceInfo = d}

    info :: Behavior (Maybe ValueInfo)
    info = _zwdInfo <&> (>>= mkValueInfo)

    blookup :: Behavior (DeviceEvent -> Maybe ValueEvent)
    blookup =
      info
        <&> (\mVi de -> do
              vi <- mVi
              a  <- extractValueInfo de
              lookup vi a
            )
     where
      extractValueInfo :: DeviceEvent -> Maybe (ValueId, ValueEvent)
      extractValueInfo (DValueChanged v s) = Just (v, VValueChanged s)
      extractValueInfo _                   = Nothing

      lookup :: ValueInfo -> (ValueId, ValueEvent) -> Maybe ValueEvent
      lookup ValueInfo { _vInfo = Value { _valueId = v' } } (v, s) =
        toMaybe (v == v') s

    changes = filterJust $ blookup <@> _zwdChanges

  return ZWaveValue {_zwvInfo = info, _zwvChanges = changes}

toMaybe :: Bool -> a -> Maybe a
toMaybe True  = Just
toMaybe False = const Nothing

distinct :: (MonadMoment m, Eq a) => Event a -> m (Event a)
distinct = fmap (filterJust . fst) . mapAccum Nothing . fmap f
 where
  f y (Just x) = if x == y then (Nothing, Just x) else (Just y, Just y)
  f y Nothing  = (Just y, Just y)

valueChanges :: ZWaveValue -> Event (ZEventSource ValueState)
valueChanges ZWaveValue {..} = filterJust . observeE $ fmap
  (valueB . toEventSource)
  _zwvChanges
 where
  toEventSource (VValueChanged s) = fmap (flip ZEventSource s) <$> mTag
  format a b = a <> ": " <> b
  mTag = liftA2 format <$> device <*> value
  device =
    fmap (Text.unpack . _deviceName . _dInfo . _vDeviceInfo) <$> _zwvInfo
  value = fmap (Text.unpack . _valueName . _vInfo) <$> _zwvInfo
