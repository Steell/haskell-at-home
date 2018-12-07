{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import           Api                     hiding ( getValue )

import           Client

import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Trans            ( lift )

import qualified Data.List                     as List
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as Text
import           Data.Time.Clock                ( UTCTime(..)
                                                , getCurrentTime
                                                )
import           Data.Time.Horizon              ( sunrise
                                                , sunset
                                                )

import           Network.HTTP.Client            ( newManager
                                                , defaultManagerSettings
                                                )
import qualified Network.Mail.SMTP             as SMTP

import           Reactive.Banana
import           Reactive.Banana.Frameworks     ( MomentIO
                                                , execute
                                                , fromPoll
                                                , reactimate
                                                )

import           ReactiveDaemon

import           Servant.Client

import           System.Environment             ( getArgs )

----

makePrisms ''ValueState

main :: IO ()
main = do
    mgr <- newManager defaultManagerSettings
    let cenv = mkClientEnv mgr (BaseUrl Http "localhost" 8081 "")
    phoneNumbers <- getArgs --TODO: make nicer
    runClient cenv $ myconfig phoneNumbers

data DoorState = Open | Closed

myconfig :: [String] -> ZWave MomentIO ()
myconfig phoneNumbers = do
    home <- getHome

    singleDimmerCfg home guestroom
    singleDimmerCfg home diningroom
    multiDimmerCfg home bedroom
    multiDimmerCfg home livingroom
    globalCfg home all
    entranceCfg home frontDoorSensor [livingroomEntry]

    let addrs = SMTP.Address Nothing . Text.pack <$> phoneNumbers
    washerCfg addrs home [washerOutlet, dryerOutlet]
  where
    livingroom@[livingroomEntry, livingroomMantle, livingroomSeating] =
        [3 .. 5]
    guestroom       = 2 --("guest bedroom / office", [("all", 2)])
    bedroom         = [6 .. 8] -- @[bedroomFront, bedroomKellie, bedroomSteve] 
    diningroom      = 9 -- ("dining room", [("all", 9)])
    washerOutlet    = 10 --("washing machine", 10)
    frontDoorSensor = 11 -- ("front door", 11)
    basement        = [12 .. 14] -- @[basementStairs, basementSeating, basementConsole, leftSpeaker, rightSpeaker] --16]
    energyMeter     = 17
    dryerOutlet     = 18
    all             = livingroom ++ bedroom ++ [diningroom]

entranceCfg :: ZWaveHome -> DeviceId -> [DeviceId] -> ZWave MomentIO ()
entranceCfg home entry lights = do
    let entryDevice = getDeviceById home entry
        lightDevs =
            getDeviceValueByName "Level" . getDeviceById home <$> lights
        doorE     = getDoorEvent entryDevice
        newLevelE = filterJust $ traverse toLevel <$> doorE
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

globalCfg :: ZWaveHome -> [DeviceId] -> ZWave MomentIO ()
globalCfg home ds = dimmerCfg home ds ds toLevel
  where
    toLevel :: Scene -> Maybe Integer
    toLevel TripleDown = Just 0
    toLevel _          = Nothing

multiDimmerCfg :: ZWaveHome -> [DeviceId] -> ZWave MomentIO ()
multiDimmerCfg home ds = dimmerCfg home ds ds $ toLevel >>> Just
  where
    toLevel :: Scene -> Integer
    toLevel DoubleUp   = 0xFF
    toLevel DoubleDown = 0
    toLevel TripleUp   = 0x63
    toLevel TripleDown = 0

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
    getDeviceValueByName "Scene Number" d
        & (   valueChanges
          >$> traverse (preview _VByte)
          >>> filterJust
          >$> traverse (flip Map.lookup sceneNumberMap)
          >>> filterJust
          )
  where
    sceneNumberMap :: Map Integer Scene
    sceneNumberMap = Map.fromList
        [(13, DoubleUp), (14, TripleUp), (23, DoubleDown), (24, TripleDown)]

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
    handleScene DoubleUp   0 = Just 0xFF -- previous level
    handleScene TripleUp   0 = Just 0xFF
    handleScene DoubleUp   _ = Just 0x63 -- max level
    handleScene TripleUp   _ = Just 0x63 -- max level
    handleScene DoubleDown _ = Just 0    -- off
    handleScene TripleDown _ = Just 0

data WashState = Active | Inactive

washerCfg :: [SMTP.Address] -> ZWaveHome -> [DeviceId] -> ZWave MomentIO ()
washerCfg addrs home ds = do
    let reactToChange Active Inactive = sendEmail addrs
        reactToChange _      _        = return ()

        -- TODO: this is a monoid...
        state' x = if x > 0.0 then Active else Inactive
        state :: [Float] -> WashState
        state lvls = if any (> 0.0) lvls then Active else Inactive
        stateMappend Active _      = Active
        stateMappend _      Active = Active
        stateMappend _      _      = Inactive

        powerEvt :: DeviceId -> (DeviceId, Event Float)
        powerEvt d =
            ( d
            , fmap (^?! eventData . _VDecimal)
                . valueChanges
                . getDeviceValueByName "Power"
                $ getDeviceById home d
            )

        powerEvts :: [(DeviceId, Event Float)]
        powerEvts = powerEvt <$> ds

        stateEvts :: [Event WashState]
        stateEvts = fmap (\(_, e) -> state' <$> e) powerEvts

    stateMapB <- accumB Map.empty (unions ((\(d, v) -> Map.insert d <$> v) <$> powerEvts))
    let stateB = reactToChange . state . Map.elems <$> stateMapB
    lift . reactimate . apply stateB $ mergeE stateMappend stateEvts

sendEmail :: [SMTP.Address] -> IO ()
sendEmail to = SMTP.renderSendMail mail
  where
    mail = SMTP.simpleMail from to [] [] subject [SMTP.plainTextPart msg]
    from =
        SMTP.Address (Just "Washing Machine") "washer@melrose.steellworks.com"
    subject = "The washing machine has finished"
    msg     = "You may now move your clothes to the dryer"
