{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Api                     hiding ( getValue )

import           Client

import           Control.Arrow
import           Control.Concurrent.AlarmClock  ( AlarmClock
                                                , setAlarm
                                                , withAlarmClock
                                                )
import           Control.Concurrent.Async       ( race_ )
import           Control.Lens
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans            ( lift )

import qualified Data.Conduit.Combinators      as Conduit
import           Data.Foldable                  ( traverse_ )
import           Data.Functor                   ( void )
import qualified Data.List                     as List
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Text                     ( Text )
import qualified Data.Text                     as Text
import           Data.Time                      ( DayOfWeek(..)
                                                , LocalTime(..)
                                                , NominalDiffTime
                                                , TimeOfDay(..)
                                                , UTCTime(..)
                                                , ZonedTime(..)
                                                , addLocalTime
                                                , dayOfWeek
                                                , daysAndTimeOfDayToTime
                                                , getCurrentTime
                                                , getTimeZone
                                                , getZonedTime
                                                , localTimeToUTC
                                                , midnight
                                                , nominalDay
                                                , secondsToNominalDiffTime
                                                )
import           Data.Time.Horizon              ( sunrise
                                                , sunset
                                                )

import           Network.HTTP.Client            ( newManager
                                                , defaultManagerSettings
                                                )
import qualified Network.Mail.SMTP             as SMTP

import           Reactive.Banana
import           Reactive.Banana.Frameworks     ( AddHandler
                                                , MomentIO
                                                , actuate
                                                , changes
                                                , execute
                                                , fromAddHandler
                                                , fromPoll
                                                , liftIOLater
                                                , newAddHandler
						, newEvent
                                                , reactimate
                                                , reactimate'
                                                )

import           ReactiveDaemon

import           Servant.Client

import           System.Environment             ( getArgs )

----

makePrisms ''ValueState

data SceneButton = Up | Down
  deriving (Show)
data SceneGesture = DoublePress | TriplePress
  deriving (Show)
type Scene = (SceneGesture, SceneButton)
makePrisms ''SceneButton
makePrisms ''SceneGesture

data Light = Light { _applyScene :: Behavior (Scene -> IO ())
                   , _device :: ZWaveDevice
                   }

main :: IO ()
main = do
    mgr <- newManager defaultManagerSettings
    let cenv = mkClientEnv mgr (BaseUrl Http "localhost" 8081 "")
    (code3:phoneNumbers) <- getArgs --TODO: make nicer

    --TODO: somehow incorporate this into myconfig
    (onStart, triggerAlarmStart) <- newAddHandler
    (onStop, triggerAlarmStop) <- newAddHandler
    withAlarmClock (\_ _ -> triggerAlarmStart ()) $ \startAlarm -> do
      withAlarmClock (\_ _ -> triggerAlarmStop ()) $ \stopAlarm -> do
        lcd <- myLcd startAlarm onStart stopAlarm onStop
        let cfg = myconfig phoneNumbers (Text.pack code3) lcd
        (eventHandler, writeEvent) <- newAddHandler
        (stateHandler, writeState) <- newAddHandler
        let client  = clientIO cenv
            netDesc = dNetwork client cfg stateHandler eventHandler
        compile netDesc >>= actuate
        startTheAlarm lcd
        race_ (thread (handleState client) writeState)
              (thread (handleEvents client) writeEvent)
  where
    thread :: (ConduitClient a () -> IO ()) -> (a -> IO ()) -> IO ()
    thread connect write = void . connect $ do
        Conduit.mapM_ (liftIO . write)

startTheAlarm :: LockCodeData -> IO ()
startTheAlarm lcd = do
      currentZonedTime <- getZonedTime
      let currentTimeZone = zonedTimeZone currentZonedTime
          (start:_) = _startTimes lcd
      setAlarm (_startAc lcd) $ localTimeToUTC currentTimeZone start

data DoorState = Open | Closed

myconfig :: [String]
         -> Text
         -> LockCodeData
         -> ZWave MomentIO ()
myconfig phoneNumbers code3 lcd = do
    home <- getHome

    let (basementStairsD : basementDimmersD) = getDeviceById home <$> basement
    basementLights <- sequence $
      mkSwitchLight basementStairsD
      : (mkDimmerLight <$> basementDimmersD)

    roomLightCfg basementLights basementLights

    singleDimmerCfg home guestroom
    singleDimmerCfg home diningroom
    multiDimmerCfg home bedroom
    multiDimmerCfg home livingroom

    let allD = getDeviceById home <$> all
    allLights <- mkDimmerLight `traverse` allD
    frontDoorLockLightIGuess <- mkLockLightIGuess $ getDeviceById home frontDoorLock

    globalCfg2 allLights (frontDoorLockLightIGuess : allLights)
--    globalCfg home all

    entranceCfg home frontDoorSensor [livingroomEntry]

    lockCodeCfg home frontDoorLock code3 lcd
    --batteryEmail addrs frontDoorLock

    let addrs = SMTP.Address Nothing . Text.pack <$> phoneNumbers
    washerCfg addrs home [ (washerOutlet, "Electric - W", 5.0)
                         , (dryerOutlet, "Instance 1: Electric - W", 50.0)
                         ]
  where
    livingroom@[livingroomEntry, _, _] = --livingroomMantle, livingroomSeating] =
        [3 .. 5]
    guestroom       = 2 --("guest bedroom / office", [("all", 2)])
    bedroom         = [6 .. 8] -- @[bedroomFront, bedroomKellie, bedroomSteve]
    diningroom      = 9 -- ("dining room", [("all", 9)])
    washerOutlet    = 10 --("washing machine", 10)
    frontDoorSensor = 11 -- ("front door", 11)
    basement = {-@(basementStairs : basementDimmers@[ basementSeating
                                               , basementConsole
                                               , leftSpeaker
                                               , rightSpeaker
                                               ]) = -}
      [12 .. 16]
--    energyMeter     = 17
    dryerOutlet     = 18
    frontDoorLock   = 21
    all             = livingroom ++ bedroom ++ [diningroom]


data LockCodeData = LockCodeData { _startAc :: AlarmClock UTCTime
                                 , _startHandler :: AddHandler ()
                                 , _endAc :: AlarmClock UTCTime
                                 , _endHandler :: AddHandler ()
                                 , _startTimes :: [LocalTime]
                                 , _endTimes :: [LocalTime]
                                 }

pairwise :: [a] -> [(a, a)]
pairwise xs = xs `zip` (drop 1 $ cycle xs)

myLcd startAc startAh stopAc stopAh = do
    currentZonedTime <- getZonedTime

    let schedule :: [(DayOfWeek, TimeOfDay, NominalDiffTime)]
        schedule = ((, TimeOfDay 10 0 0, hoursToNominalDiffTime 6) <$> [Monday, Tuesday, Thursday])

        currentLocalTime = zonedTimeToLocalTime currentZonedTime
        currentDay       = localDay currentLocalTime
        currentTimeOfDay = localTimeOfDay currentLocalTime
        currentDayOfWeek = dayOfWeek currentDay
        currentTimeZone  = zonedTimeZone currentZonedTime

        toNominal = daysAndTimeOfDayToTime 0

        past (dow, tod, d) =
          (fromEnum dow) < (fromEnum currentDayOfWeek)
            || dow == currentDayOfWeek && toNominal tod + d < toNominal currentTimeOfDay

        schedule0 = dropWhile past schedule
        schedule' = schedule0 <> cycle schedule

        getScheduledZonedTime :: (LocalTime, NominalDiffTime)
                              -> (DayOfWeek, TimeOfDay, NominalDiffTime)
                              -> (LocalTime, NominalDiffTime)
        getScheduledZonedTime (prevLocalTime, _) (dow, tod, duration) =
          let prevDay       = localDay prevLocalTime
              prevDayOfWeek = dayOfWeek prevDay
              prevTimeOfDay = localTimeOfDay prevLocalTime
              diffDay       = toInteger $ dowDiff prevDayOfWeek dow
              nextLocalTime = daysAndTimeOfDayToTime diffDay tod
                                `addLocalTime` (prevLocalTime { localTimeOfDay = midnight })
          in
            (nextLocalTime, duration)

        schedule'' = tail $ List.scanl' getScheduledZonedTime (currentLocalTime, undefined) schedule'
        schedule''' = fmap (\(lt, diff) -> (lt, diff `addLocalTime` lt)) schedule''

        startTimes = fmap fst schedule'''
        endTimes   = undefined : fmap snd schedule'''

    return LockCodeData { _startAc = startAc
                        , _startHandler = startAh
                        , _endAc = stopAc
                        , _endHandler = stopAh
                        , _startTimes = startTimes
                        , _endTimes = endTimes
                        }
  where
    hoursToNominalDiffTime h = secondsToNominalDiffTime $ h * 60 * 60
    dowDiff a b = ((fromEnum b) - (fromEnum a)) `mod` 7

lockCodeCfg :: ZWaveHome
            -> DeviceId
            -> Text
            -> LockCodeData
            -> ZWave MomentIO ()
lockCodeCfg home lock code3 LockCodeData{..} = do

    setter <- view zwSetValue

    startE <- lift $ fromAddHandler _startHandler
    stopE <- lift $ fromAddHandler _endHandler
    stopTimesE  <- accumE _endTimes (tail <$ startE) <&> fmap head
    startTimesE <- accumE _startTimes (tail <$ stopE) <&> fmap head

    let setValue s vInfo =
	  void $ setter (_dHomeId $ _vDeviceInfo vInfo) lock (_valueId $ _vInfo vInfo) s
	codeVal = getDeviceValueByName "Code 3:" $ getDeviceById home lock
	codeSetter = codeVal & _zwvInfo <&> fmap (setValue (VString code3))
	resetVal = getDeviceValueByName "Remove User Code" $ getDeviceById home lock
	codeResetter = resetVal & _zwvInfo <&> fmap (setValue (VShort 3))

        -- TODO: would be great if we didn't have to use changes' here
        changes' b = do
	  (e, handle) <- newEvent
          eb <- changes b
          reactimate' $ (fmap handle) <$> eb
          return e

    codeSetterE   <- lift $ changes' codeSetter
    codeResetterE <- lift $ changes' codeResetter

    let enableCodeE  = codeSetterE   <&> updateCodeSetter
	disableCodeE = codeResetterE <&> updateCodeResetter
	onStartedE   = stopTimesE    <&> enableCodeForDuration
	onStoppedE   = startTimesE   <&> disableCodeForDuration

    (actionE, _) <- mapAccum initialState
      $ mergeE const [ enableCodeE, disableCodeE, onStartedE, onStoppedE ]

    lift $ reactimate $ filterJust actionE

  where
    updateCodeSetter setter state@(LockCfgState {..}) =
      let action = case (setter, _codeSetter, _duration, _enabled) of
                     (Just s, Nothing, Just d, True) -> Just (doEnable d s)
                     _                               -> Nothing
      in
        (action, state { _codeSetter = setter })

    updateCodeResetter resetter state@(LockCfgState {..}) =
      let action = case (resetter, _codeResetter, _duration, _enabled) of
                     (Just r, Nothing, Just d, False) -> Just (doDisable d r)
                     _                                -> Nothing
      in
        (action, state { _codeResetter = resetter})

    enableCodeForDuration duration state@(LockCfgState {..}) =
      let action = doEnable duration <$> _codeSetter
      in
        (action, state { _duration = Just duration, _enabled = True })

    disableCodeForDuration duration state@(LockCfgState {..}) =
      let action = doDisable duration <$> _codeResetter
      in
        (action, state { _duration = Just duration, _enabled = False })

    initialState = LockCfgState { _enabled = False
                                , _duration = Nothing
                                , _codeSetter = Nothing
                                , _codeResetter = Nothing
                                }

    doDisable t reset = do
      print "CODE OFF"
      reset
      zone <- getTimeZone =<< getCurrentTime
      let startTime = localTimeToUTC zone t
      print "Starting at:"
      print $ "  local: " <> show t
      print $ "  zoned: " <> show startTime
      setAlarm _startAc $ startTime
      
    doEnable t set = do
      print "CODE ON"
      set
      zone <- getTimeZone =<< getCurrentTime
      let startTime = localTimeToUTC zone t
      print "Stopping at:"
      print $ "  local: " <> show t
      print $ "  zoned: " <> show startTime
      setAlarm _endAc $ startTime

data LockCfgState = LockCfgState { _enabled :: Bool
                                 , _duration :: Maybe LocalTime
                                 , _codeSetter :: Maybe (IO ())
                                 , _codeResetter :: Maybe (IO ())
                                 }

(?:) :: Maybe a -> a -> a
Just a  ?: _ = a
Nothing ?: a = a

entranceCfg :: ZWaveHome -> DeviceId -> [DeviceId] -> ZWave MomentIO ()
entranceCfg home entry lights = do
    let entryDevice = getDeviceById home entry
        lightDevs   = getDeviceValueByName "Level" . getDeviceById home <$> lights
        doorE       = getDoorEvent entryDevice
        newLevelE   = filterJust $ traverse toLevel <$> doorE
        toLevel Closed = Nothing
        toLevel Open   = Just 0xFF -- last setting

    currentTime <- lift $ fromPoll getCurrentTime
    nightEvents <- lift $ unlessE isSunOut currentTime "isDaytime: " newLevelE
    (fmap VByte <$> nightEvents) ~$> lightDevs

-- unlessE :: Behavior Bool -> Event a -> Event a
-- unlessE = whenE . fmap not
unlessE
    :: (Show a, Show b)
    => (b -> Bool)
    -> Behavior b
    -> String
    -> Event (ZEventSource a)
    -> MomentIO (Event (ZEventSource a))
unlessE f b reason evt = fmap filterJust . execute $ evt <&> \e -> do
    b' <- valueB b
    if f b'
        then do
            liftIO
                .  putStrLn
                $  "(SUPPRESS) "
                ++ show e
                ++ " :: "
                ++ reason
                ++ show b'
            return Nothing
        else return $ Just e

isSunOutB :: MomentIO (Behavior Bool)
isSunOutB = fromPoll $ isSunOut <$> getCurrentTime

isSunOut :: UTCTime -> Bool
isSunOut (UTCTime day time) =
    let (UTCTime _ morning) = sunrise day 42.458429 (-71.066163)
        (UTCTime _ evening) = sunset day 42.458429 (-71.066163)
    in  time >= morning && time <= evening

mkLockLightIGuess :: Monad m => ZWaveDevice -> ZWave m Light
mkLockLightIGuess _device = do
    setter <- view zwSetValue
    let _applyScene = applyScene setter
    return Light {..}
  where
    levelVal = getDeviceValueByName "Locked" _device

    applyScene setter = _zwvInfo levelVal <&> \case
      Nothing -> const $ pure ()
      (Just ValueInfo {..}) -> \scene -> case toLevel scene of
        Just b -> do
          void $ setter (_dHomeId _vDeviceInfo)
            (_deviceId $ _dInfo _vDeviceInfo)
            (_valueId _vInfo)
            (VBool b)
	Nothing -> pure ()

    toLevel :: Scene -> Maybe Bool
    toLevel (TriplePress, Down) = Just True
    toLevel _                   = Nothing


mkDimmerLight :: Monad m => ZWaveDevice -> ZWave m Light
mkDimmerLight _device = do
    setter <- view zwSetValue
    let _applyScene = applyScene setter
    return Light {..}
  where
    levelVal = getDeviceValueByName "Level" _device

    applyScene setter = _zwvInfo levelVal <&> \case
      Nothing -> const $ pure ()
      (Just ValueInfo {..}) -> \scene ->
        void $ setter (_dHomeId _vDeviceInfo)
          (_deviceId $ _dInfo _vDeviceInfo)
          (_valueId _vInfo)
          (VByte $ toLevel scene)

    toLevel :: Scene -> Integer
    toLevel (DoublePress, Up)   = 0xFF
    toLevel (DoublePress, Down) = 0
    toLevel (TriplePress, Up)   = 0x63
    toLevel (TriplePress, Down) = 0

mkSwitchLight :: Monad m => ZWaveDevice -> ZWave m Light
mkSwitchLight _device = do
    setter <- view zwSetValue
    let _applyScene = applyScene setter
    return Light {..}
  where
    switchVal = getDeviceValueByName "Switch" _device

    applyScene setter = _zwvInfo switchVal <&> \case
      Nothing -> const $ pure ()
      (Just ValueInfo {..}) -> \scene ->
        void $ setter (_dHomeId _vDeviceInfo)
          (_deviceId $ _dInfo _vDeviceInfo)
          (_valueId _vInfo)
          (VBool $ toLevel scene)

    toLevel (_, Up) = True
    toLevel _       = False

roomLightCfg :: [Light] -> [Light] -> ZWave MomentIO ()
roomLightCfg ins outs = do
    let sceneE = mergeE const $ fmap (getSceneEvent . _device) ins
        outCfg :: Light -> Event (IO ())
        outCfg l = (\f -> f . _eventData) <$> _applyScene l <@> sceneE
    lift $ traverse_ (reactimate . outCfg) outs

dimmerCfg
    :: ZWaveHome
    -> [DeviceId]
    -> [DeviceId]
    -> (Scene -> Maybe Integer)
    -> ZWave MomentIO ()
dimmerCfg home ins outs toLevel = do
    let sourceDevices    = getDeviceById home <$> ins
        targetDevices    = getDeviceById home <$> outs
        getSetLevelEvent = getSceneEvent >$> traverse toLevel >>> filterJust
        newLevelEvent =
            fmap VByte <$> mergeE const (getSetLevelEvent <$> sourceDevices)

    newLevelEvent ~$> (getDeviceValueByName "Level" <$> targetDevices)

(>$>) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
ff >$> g = fmap g . ff

(<$<) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
(<$<) = flip (>$>)

globalCfg2 = roomLightCfg

globalCfg :: ZWaveHome -> [DeviceId] -> ZWave MomentIO ()
globalCfg home ds = dimmerCfg home ds ds toLevel
  where
    toLevel :: Scene -> Maybe Integer
    toLevel (TriplePress, Down) = Just 0
    toLevel _                   = Nothing

multiDimmerCfg :: ZWaveHome -> [DeviceId] -> ZWave MomentIO ()
multiDimmerCfg home ds = dimmerCfg home ds ds $ toLevel >>> Just
  where
    toLevel :: Scene -> Integer
    toLevel (DoublePress, Up)   = 0xFF
    toLevel (DoublePress, Down) = 0
    toLevel (TriplePress, Up)   = 0x63
    toLevel (TriplePress, Down) = 0

mergeE :: (b -> b -> b) -> [Event b] -> Event b
mergeE f = List.foldl' (unionWith f) never

getDoorEvent :: ZWaveDevice -> Event (ZEventSource DoorState)
getDoorEvent d =
    getDeviceValueByName "Sensor" d
        & (   valueChanges
          >$> traverse (preview _VBool)
          >>> filterJust
          >$> fmap lookup
          )
  where
    lookup True  = Open
    lookup False = Closed

getSceneEvent :: ZWaveDevice -> Event (ZEventSource Scene)
getSceneEvent d =
  let upEvents = getDeviceValueByName "Scene 1" d
      downEvents =  getDeviceValueByName "Scene 2" d
  in
    unionWith const (extractScene upEvents Up) (extractScene downEvents Down)
  where
    extractScene sceneE sceneButton = sceneE
      & (   valueChanges
        >$> traverse (preview (_VList . _1))
        >>> filterJust
        >$> traverse (flip Map.lookup sceneGestureMap)
        >>> filterJust
        >$> fmap (\gesture -> (gesture, sceneButton))
        )

    sceneGestureMap :: Map Int  SceneGesture
    sceneGestureMap = Map.fromList [(4, DoublePress), (5, TriplePress)]

singleDimmerCfg :: ZWaveHome -> DeviceId -> ZWave MomentIO ()
singleDimmerCfg home d = do
    let device   = getDeviceById home d
        levelV   = getDeviceValueByName "Level" device
        sceneEvt = getSceneEvent device

        handlerB :: Behavior (Scene -> Maybe Integer)
        handlerB = handler <$> getValue levelV
          where
            handler mv s = mv ^? _Just . _VByte . to (handleScene s) . _Just

        newLevelEvt :: Event (ZEventSource ValueState)
        newLevelEvt =
            fmap (fmap VByte) . filterJust $ traverse <$> handlerB <@> sceneEvt

    newLevelEvt ~> levelV
  where
    handleScene :: Scene -> Integer -> Maybe Integer
    handleScene (DoublePress, Up)   0 = Just 0xFF -- previous level
    handleScene (TriplePress, Up)   0 = Just 0xFF
    handleScene (DoublePress, Up)   _ = Just 0x63 -- max level
    handleScene (TriplePress, Up)   _ = Just 0x63 -- max level
    handleScene (DoublePress, Down) _ = Just 0    -- off
    handleScene (TriplePress, Down) _ = Just 0

data WashState = Active | Inactive

washerCfg :: [SMTP.Address] -> ZWaveHome -> [(DeviceId, String, Float)] -> ZWave MomentIO ()
washerCfg addrs home ds = do
    let reactToChange (os, Active) (ns, Inactive) = do
            putStrLn ("WASH COMPLETE: " ++ show (merge (Map.fromList os) (Map.fromList ns)))
            sendEmail addrs
        reactToChange (os, Inactive) (ns, Active) =
            putStrLn ("WASH STARTED: " ++ show (merge (Map.fromList os) (Map.fromList ns)))
        reactToChange _      _        = return ()

        merge :: Map DeviceId Float -> Map DeviceId Float -> Map DeviceId String
        merge = Map.intersectionWith (\o n -> show o ++ " ==> " ++ show n)

        state :: Float -> Float -> WashState
        state threshold lvl | threshold < lvl = Active
                            | otherwise       = Inactive

        powerEvt :: (DeviceId, String, Float)
                 -> Event (Map DeviceId (Float, Float)
                 -> Map DeviceId (Float, Float))
        powerEvt (d, vname, t) =
            Map.insert d
                <$> ( fmap (\x -> (t, x ^?! eventData . _VDecimal))
                    . valueChanges
                    . getDeviceValueByName vname
                    $ getDeviceById home d
                    )

        updateState new (_, old) = (old, new)

        -- TODO: Monoid
        reduce :: ([a], WashState) -> (a, WashState) -> ([a], WashState)
        reduce (is, Inactive) (i, Inactive) = ((i:is), Inactive)
        reduce (is, _)        (i, _)        = ((i:is), Active)

    stateMapE    <- accumE Map.empty . unions $ powerEvt <$> ds
    stateChangeE <-
        accumE (([], Inactive), ([], Inactive))
        $   updateState
        .   List.foldl' reduce ([], Inactive)
        .   fmap (\(d, vs@(_,v)) -> ((d, v), uncurry state vs))
        .   Map.toList
        <$> stateMapE
    lift . reactimate $ uncurry reactToChange <$> stateChangeE

sendEmail :: [SMTP.Address] -> IO ()
sendEmail to = SMTP.renderSendMail mail
  where
    mail    = SMTP.simpleMail from to [] [] subject [SMTP.plainTextPart msg]
    from = SMTP.Address (Just "Laundry Room") "washer@melrose.steellworks.com"
    subject = "The laundry has finished"
    msg     = "Move your ass and go get it"
