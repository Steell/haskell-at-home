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

import           Control.Lens
import           Control.Monad
import           Control.Monad.Reader

import           Data.Foldable                  ( find
                                                , traverse_
                                                )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( listToMaybe )
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
                     , _zwHome :: Behavior (Maybe HomeId)
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
makeLenses ''ZEventSource

instance Show a => Show (ZEventSource a) where
  show ZEventSource {..} = _eventTag <> ": " <> show _eventData

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

mapMaybe :: (a -> Maybe b) -> Event a -> Event b
mapMaybe f = filterJust . fmap f

zwData
  :: MonadMoment m => Client IO API -> Event HomeMap -> Event ZEvent -> m ZWData
zwData client eState events = do
  let setValue' = setValueState client
  bHomeMap <- stepper Map.empty eState
  let homeIdE = fmap fst . listToMaybe . Map.toList <$> eState
  bHomeId <- stepper Nothing homeIdE
  return $ ZWData bHomeMap events setValue' bHomeId

getValue :: ZWaveValue -> Behavior (Maybe ValueState)
getValue ZWaveValue {..} = fmap (_valueState . _vInfo) <$> _zwvInfo

(~>) :: Event (ZEventSource ValueState) -> ZWaveValue -> ZWave MomentIO ()
(~>) = setValueOnEvent

(~$>)
  :: Foldable f
  => Event (ZEventSource ValueState)
  -> f ZWaveValue
  -> ZWave MomentIO ()
e ~$> fv = setValueOnEvent e `traverse_` fv

setValueOnEvent
  :: Event (ZEventSource ValueState) -> ZWaveValue -> ZWave MomentIO ()
setValueOnEvent events ZWaveValue {..} = do
  setter <- view zwSetValue
  let getSetter :: Maybe ValueInfo -> ZEventSource ValueState -> IO ()
      getSetter Nothing                 _     = pure ()
      getSetter (Just v@ValueInfo {..}) event = do
        log v event
        void $ setter (_dHomeId _vDeviceInfo)
                      (_deviceId $ _dInfo _vDeviceInfo)
                      (_valueId _vInfo)
                      (_eventData event)
  lift $ reactimate $ getSetter <$> _zwvInfo <@> events
 where
  log ValueInfo {..} e =
    putStrLn
      $  show e
      <> " => "
      <> "{ device: "
      <> Text.unpack (_deviceName $ _dInfo _vDeviceInfo)
      <> ", valueName: \""
      <> Text.unpack (_valueName _vInfo)
      <> "\", oldValue: "
      <> show (_valueState _vInfo)
      <> " }"

getHome :: Monad m => ZWave m ZWaveHome
getHome = do
  bHomeMap <- view zwMap
  eChange  <- view zwChanges
  bHomeId  <- view zwHome
  let changes =
        filterJust
          $   maybe (const . const Nothing) toHomeEvent
          <$> bHomeId
          <*> bHomeMap
          <@> eChange
  return ZWaveHome
    { _zwhInfo    = maybe (const Nothing) Map.lookup <$> bHomeId <*> bHomeMap
    , _zwhChanges = changes
    }
 where
  toHomeEvent :: HomeId -> HomeMap -> ZEvent -> Maybe HomeEvent
  toHomeEvent h hmap = go
   where
    go (DeviceAdded h' d) | h == h' = Just . HDeviceAdded $ DeviceInfo d h
    go (DeviceRemoved h' d) | h == h' = Just $ HDeviceRemoved d
    go (ValueAdded h' deviceId v) | h == h' = do
      home <- Map.lookup h' hmap
      d    <- Map.lookup deviceId $ _homeDevices home
      return . HValueAdded deviceId . ValueInfo v $ DeviceInfo d h
    go (ValueRemoved h' d v) | h == h' = Just $ HValueRemoved d v
    go (ValueChanged h' d v s) | h == h' = Just $ HValueChanged d v s
    go _                               = Nothing

getDeviceById :: ZWaveHome -> DeviceId -> ZWaveDevice
getDeviceById ZWaveHome {..} deviceId = ZWaveDevice
  { _zwdInfo    = info
  , _zwdChanges = changes
  }
 where
  mkInfo :: Home -> Maybe DeviceInfo
  mkInfo Home {..} =
    flip DeviceInfo _homeId <$> Map.lookup deviceId _homeDevices

  info :: Behavior (Maybe DeviceInfo)
  info = _zwhInfo <&> (>>= mkInfo)

  changes :: Event DeviceEvent
  changes = filterJust $ _zwhChanges <&> toDeviceEvent

  toDeviceEvent :: HomeEvent -> Maybe DeviceEvent
  toDeviceEvent (HValueAdded d v) | d == deviceId = Just $ DValueAdded v
  toDeviceEvent (HValueRemoved d v) | d == deviceId = Just $ DValueRemoved v
  toDeviceEvent (HValueChanged d v s) | d == deviceId = Just $ DValueChanged v s
  toDeviceEvent _ = Nothing

getDeviceValueByName :: String -> ZWaveDevice -> ZWaveValue
getDeviceValueByName name ZWaveDevice {..} = ZWaveValue
  { _zwvInfo    = info
  , _zwvChanges = changes
  }
 where
  mkValueInfo :: DeviceInfo -> Maybe ValueInfo
  mkValueInfo d@DeviceInfo {..} = do
    v <- find ((== name) . Text.unpack . _valueName) . Map.elems $ _deviceValues
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

  changes :: Event ValueEvent
  changes = filterJust $ blookup <@> _zwdChanges

  extractValueInfo :: DeviceEvent -> Maybe (ValueId, ValueEvent)
  extractValueInfo (DValueChanged v s) = Just (v, VValueChanged s)
  extractValueInfo _                   = Nothing

  lookup :: ValueInfo -> (ValueId, ValueEvent) -> Maybe ValueEvent
  lookup ValueInfo { _vInfo = Value { _valueId = v' } } (v, s) =
    toMaybe (v == v') s

  toMaybe :: Bool -> a -> Maybe a
  toMaybe True  = Just
  toMaybe False = const Nothing

valueChanges :: ZWaveValue -> Event (ZEventSource ValueState)
valueChanges ZWaveValue {..} =
  filterJust . observeE $ valueB . toEventSource <$> _zwvChanges
 where
  toEventSource (VValueChanged s) = fmap (flip ZEventSource s) <$> mTag
  format a b = a <> ": " <> b
  mTag = liftA2 format <$> device <*> value
  device =
    fmap (Text.unpack . _deviceName . _dInfo . _vDeviceInfo) <$> _zwvInfo
  value = fmap (Text.unpack . _valueName . _vInfo) <$> _zwvInfo
