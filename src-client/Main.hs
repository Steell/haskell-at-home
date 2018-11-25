{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Api                     hiding ( getValue )

import           Client

import           Control.Arrow
import           Control.Lens
import           Control.Monad
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
    home <- getHomeById 4171812579

    singleDimmerCfg home guestroom
    singleDimmerCfg home diningroom
    multiDimmerCfg home bedroom
    multiDimmerCfg home livingroom
    globalCfg home all
    entranceCfg home frontDoorSensor [livingroomEntry]

    let addrs = SMTP.Address Nothing . Text.pack <$> phoneNumbers
    washerCfg addrs home washerOutlet
    
  where
    livingroom@[livingroomEntry, livingroomMantle, livingroomSeating] =
        [3 .. 5]
    guestroom       = 2 --("guest bedroom / office", [("all", 2)])
    bedroom@[bedroomFront, bedroomKellie, bedroomSteve] = [6 .. 8]
    diningroom      = 9 -- ("dining room", [("all", 9)])
    all             = livingroom ++ bedroom ++ [diningroom]
    washerOutlet    = 10 --("washing machine", 10)
    frontDoorSensor = 11 -- ("front door", 11)
    basement@[basementStairs, basementSeating, basementConsole] = [12 .. 14]

entranceCfg :: ZWaveHome -> DeviceId -> [DeviceId] -> ZWave MomentIO ()
entranceCfg home entry lights = do
    entryDevice <- getDeviceById entry home
    lightDevs   <- mapM
        (flip getDeviceById home >=> getDeviceValueByName "Level")
        lights
    doorE <- getDoorEvent entryDevice
    sunB  <- lift isSunOutB

    let setLevelsOnEvt e v = setValueOnEvent v $ fmap (fmap VByte) e
        newLevelEvt =
            whenE (not <$> sunB) . filterJust $ traverse toLevel <$> doorE
        toLevel Closed = Nothing
        toLevel Open   = Just 0xFF -- last setting

    mapM_ (setLevelsOnEvt newLevelEvt) lightDevs

isSunOutB :: MomentIO (Behavior Bool)
isSunOutB = fromPoll getCurrentTime <&> fmap isSunOut
  where
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
    sourceDevices <- mapM (flip getDeviceById home) ins
    targetDevices <- mapM (flip getDeviceById home) outs

    let setLevelsOnEvt :: Event (ZEventSource Integer) -> ZWave MomentIO ()
        setLevelsOnEvt byteE =
            targetDevices
                `forM_` (   getDeviceValueByName "Level"
                        >=> flip setValueOnEvent (fmap VByte <$> byteE)
                        )

        events :: ZWave MomentIO [Event (ZEventSource Integer)]
        events =
            sequence
                $   sourceDevices
                <&> (   getSceneEvent
                    >$> (fmap (fmap toLevel >>> sequence) >>> filterJust)
                    )

    newLevelEvt <- mergeE const <$> events
    setLevelsOnEvt newLevelEvt

(>$>) :: Functor f => (a -> f b) -> (b -> c) -> (a -> f c)
ff >$> g = fmap g . ff

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

getDoorEvent
    :: MonadMoment m => ZWaveDevice -> ZWave m (Event (ZEventSource DoorState))
getDoorEvent d =
    getDeviceValueByName "Access Control" d
        <&> (   valueChanges
            >$> (fmap (preview _VByte) >>> sequence)
            >>> filterJust
            >$> (fmap lookup >>> sequence)
            >>> filterJust
            )
  where
    lookup 22 = Just Open
    lookup 23 = Just Closed
    lookup _  = Nothing

getSceneEvent
    :: MonadMoment m => ZWaveDevice -> ZWave m (Event (ZEventSource Scene))
getSceneEvent d =
    getDeviceValueByName "Scene Number" d
        <&> (   valueChanges
            >$> (fmap (preview _VByte) >>> sequence)
            >>> filterJust
            >$> (fmap (flip Map.lookup sceneNumberMap) >>> sequence)
            >>> filterJust
            )
  where
    sceneNumberMap :: Map Integer Scene
    sceneNumberMap = Map.fromList
        [(13, DoubleUp), (14, TripleUp), (23, DoubleDown), (24, TripleDown)]

singleDimmerCfg :: ZWaveHome -> DeviceId -> ZWave MomentIO ()
singleDimmerCfg home d = do
    device   <- getDeviceById d home
    levelV   <- getDeviceValueByName "Level" device
    sceneEvt <- getSceneEvent device

    let handlerB :: Behavior (Scene -> Maybe Integer)
        handlerB =
            getValue levelV
                <&> ((>>= (preview _VByte >>> fmap handleScene)) >>> liftM)
        newLevelEvt =
            fmap (fmap VByte)
                .   filterJust
                .   fmap sequence
                $   fmap
                <$> handlerB
                <@> sceneEvt

    setValueOnEvent levelV newLevelEvt
  where
    handleScene :: Integer -> Scene -> Maybe Integer
    handleScene 0 DoubleUp   = Just 0xFF -- preview level
    handleScene 0 TripleUp   = Just 0xFF
    handleScene _ DoubleUp   = Just 0x63 -- max level
    handleScene _ TripleUp   = Just 0x63 -- max level
    handleScene _ DoubleDown = Just 0  -- off
    handleScene _ TripleDown = Just 0

    liftM :: Monad m => m (a -> m b) -> a -> m b
    liftM ff a = ($ a) =<< ff

washerCfg :: [SMTP.Address] -> ZWaveHome -> DeviceId -> ZWave MomentIO ()
washerCfg addrs home d = do
    device <- getDeviceById d home
    powerV <- getDeviceValueByName "Power" device
    let powerE = (^?! eventData . _VDecimal) <$> valueChanges powerV
    powerB <- stepper 0 powerE

    let reactToChange :: Float -> Float -> IO ()
        reactToChange old new = when (old > 0 && new < 0.5) $ sendEmail addrs

    lift . reactimate $ (powerB <&> reactToChange) <@> powerE

sendEmail :: [SMTP.Address] -> IO ()
sendEmail to = SMTP.sendMail "localhost" mail
  where
    mail = SMTP.simpleMail from to [] [] subject [SMTP.plainTextPart msg]
    from =
        SMTP.Address (Just "Washing Machine") "washer@melrose.steellworks.com"
    subject = "The washing machine has finished"
    msg     = "You may now move your clothes to the dryer"
