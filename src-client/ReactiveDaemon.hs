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

data ValueInfo = ValueInfo { _vInfo :: Value
                           , _vHomeId :: HomeId
                           , _vDeviceId :: DeviceId
                           }
  deriving (Show)
makeLenses ''ValueInfo
data ValueEvent = VValueChanged ValueState
data ZWaveValue = ZWaveValue { _zwvInfo    :: Behavior (Maybe ValueInfo)
                             , _zwvChanges :: Event ValueEvent
                             }
makeLenses ''ZWaveValue

data DeviceInfo = DeviceInfo { _dInfo :: Device
                             , _dHomeId :: HomeId
                             }
  deriving (Show)
makeLenses ''DeviceInfo
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

dNetwork
  :: Client IO API
  -> ZWave Moment (Event (IO ())) --TODO: just use MomentIO and let user cfg have access to reactimate?
  -> AddHandler HomeMap
  -> AddHandler ZEvent
  -> MomentIO ()
dNetwork client cfg stateHandler evtHandler = do
  eState <- fromAddHandler stateHandler
  eEvt   <- fromAddHandler evtHandler
  zd     <- zwData client eState eEvt
  eCfg   <- liftMoment . flip runReaderT zd $ unzwio cfg
  reactimate eCfg

zwData :: MonadMoment m => Client IO API -> Event HomeMap -> Event ZEvent -> m ZWData
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
    , _zwhChanges = filterJust $ toHomeEvent <$> eChange
    }
 where
  toHomeEvent :: ZEvent -> Maybe HomeEvent
  toHomeEvent (DeviceAdded   h' d)     | h == h' = Just . HDeviceAdded $ DeviceInfo d h
  toHomeEvent (DeviceRemoved h' d)     | h == h' = Just $ HDeviceRemoved d
  toHomeEvent (ValueAdded    h' d v)   | h == h' = Just . HValueAdded d $ ValueInfo v d h
  toHomeEvent (ValueRemoved  h' d v)   | h == h' = Just $ HValueRemoved d v
  toHomeEvent (ValueChanged  h' d v s) | h == h' = Just $ HValueChanged d v s
  toHomeEvent _ = Nothing

getDeviceById :: MonadMoment m => DeviceId -> ZWaveHome -> ZWave m ZWaveDevice
getDeviceById deviceId ZWaveHome {..} = do
  let
    mkInfo Home {..} = do
      d <- Map.lookup deviceId _homeDevices
      return $ DeviceInfo d _homeId

    info :: Behavior (Maybe DeviceInfo)
    info = _zwhInfo <&> (>>= mkInfo)

    changes :: Event DeviceEvent
    changes = filterJust $ _zwhChanges <&> toDeviceEvent

  --changes' <- {- distinct-} changes --TODO
  return ZWaveDevice {_zwdInfo = info, _zwdChanges = changes}
 where
  toDeviceEvent :: HomeEvent -> Maybe DeviceEvent
  toDeviceEvent (HValueAdded   d v)   | d == deviceId = Just $ DValueAdded v
  toDeviceEvent (HValueRemoved d v)   | d == deviceId = Just $ DValueRemoved v
  toDeviceEvent (HValueChanged d v s) | d == deviceId = Just $ DValueChanged v s
  toDeviceEvent _ = Nothing

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

      blookup :: Behavior (DeviceEvent -> Maybe ValueEvent)
      blookup = info <&> (\mVi de -> do 
                               vi <- mVi 
                               a <- extractValueInfo de
                               lookup vi a)
        where
          extractValueInfo :: DeviceEvent -> Maybe (ValueId, ValueEvent)
          extractValueInfo (DValueChanged v s) = Just (v, VValueChanged s)
          extractValueInfo _ = Nothing

          lookup :: ValueInfo -> (ValueId, ValueEvent) -> Maybe ValueEvent
          lookup ValueInfo {_vInfo=Value{_valueId=v'}} (v, s) = toMaybe (v == v') s

      changes = filterJust $ blookup <@> _zwdChanges

  --changes' <- {- distinct -} changes --TODO
  return ZWaveValue {_zwvInfo = info, _zwvChanges = changes}

toMaybe :: Bool -> a -> Maybe a
toMaybe True = Just
toMaybe False = const Nothing

distinct :: (MonadMoment m, Eq a) => Event a -> m (Event a)
distinct = fmap (filterJust . fst) . mapAccum Nothing . fmap f
 where
  f y (Just x) = if x == y then (Nothing, Just x) else (Just y, Just y)
  f y Nothing  = (Just y, Just y)

{-

TODO: need to account for events that might not originate from the ZWave graph

data ZEventSource a = ZEventSource { _eventValueInfo :: ValueInfo
                                   , _eventDeviceInfo :: DeviceInfo
                                   , _eventHomeInfo :: HomeInfo
                                   , _eventData :: a
                                   }

instance Functor ZEventSource where
  fmap f es = es { _eventData = f (_eventData es) } 

-}

valueChanges :: ZWaveValue -> Event ValueState
valueChanges = fmap (\(VValueChanged s) -> s) . _zwvChanges