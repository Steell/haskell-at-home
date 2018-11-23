{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Api                     hiding ( getValue )

import           Client2

import           Control.Arrow
import           Control.Lens
import           Control.Monad

import qualified Data.List                     as List
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as Text

import           Network.HTTP.Client            ( newManager
                                                , defaultManagerSettings
                                                )
import qualified Network.Mail.SMTP             as SMTP
import qualified Network.Mail.SMTP.Types       as SMTP

import           Reactive.Banana

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

type ZWave' = ZWave Moment

data DoorState = Open | Closed

myconfig :: [String] -> ZWave' (Event (IO ()))
myconfig phoneNumbers = do
    home         <- getHomeById 4171812579
    let addrs = fmap
            (SMTP.Address Nothing . Text.pack . (<> "@msg.fi.google.com"))
            phoneNumbers
    List.foldl' (unionWith (>>)) never <$> sequenceA
        [ singleDimmerCfg home guestroom
        , singleDimmerCfg home diningroom
        , multiDimmerCfg home bedroom
        , multiDimmerCfg home livingroom
        , globalCfg home all
        -- , entranceCfg frontDoorSensor [livingroomEntry]
        , washerCfg addrs home washerOutlet
        ]
  where
    livingroom        = [livingroomEntry, livingroomMantle, livingroomSeating]
    livingroomEntry   = 3
    livingroomMantle  = 4
    livingroomSeating = 5
    guestroom         = 2
    bedroom           = [bedroomFront, bedroomKellie, bedroomSteve]
    bedroomFront      = 6
    bedroomKellie     = 7
    bedroomSteve      = 8
    diningroom        = 9
    all               = [3 .. 9]
    washerOutlet      = 10
    frontDoorSensor   = 11
    basementStairs    = 12
    basementSeating   = 13
    basementConsole   = 14

{-
entranceCfg :: DeviceId -> [DeviceId] -> ZWave Moment (Event (IO ()))
entranceCfg entry lights = do
    entryDevice <- getDeviceById entry
    lightDevs <- mapM getDeviceById lights

    let setLevelsOnEvt byte = List.foldl (unionWith (>>)) never $
            flip setValueByteOnEvt byte .
	        getDeviceValueByName "Level" <$>
		lightDevs
        newLevelEvt = entryDevice & (getDoorEvent >>> fmap toLevel >>> filterJust)
        toLevel Closed = Nothing
	toLevel Open = Just 0xFF -- last setting

    sunB <- isSunOut
    return . setLevelsOnEvt $ whenE (not <$> sunB) newLevelEvt

isSunOut :: Monad m => ZWave m (Behavior Bool)
isSunOut = currentTimeB <&> fmap (\ (UTCTime day time) ->
    let (UTCTime _ morning) = sunrise day 42.458429 (-71.066163)
        (UTCTime _ evening) = sunset day 42.458429 (-71.066163)
    in
        time >= morning && time <= evening)
-}

dimmerCfg
    :: ZWaveHome
    -> [DeviceId]
    -> [DeviceId]
    -> (Scene -> Maybe Integer)
    -> ZWave Moment (Event (IO ()))
dimmerCfg home ins outs toLevel = do
    inDevs  <- mapM (flip getDeviceById home) ins
    outDevs <- mapM (flip getDeviceById home) outs

    let setLevelsOnEvt :: Event Integer -> ZWave' (Event (IO ()))
        setLevelsOnEvt byteE = List.foldl (unionWith (>>)) never <$> sequence
            (   outDevs
            <&> (   getDeviceValueByName "Level"
                >=> flip setValueByteOnEvt byteE
                )
            )

        events :: ZWave' [Event Integer]
        events =
            sequence
                $   inDevs
                <&> (getSceneEvent >$> (fmap toLevel >>> filterJust))

    newLevelEvt <- mergeE const <$> events
    setLevelsOnEvt newLevelEvt

(>$>) :: Functor f => (a -> f b) -> (b -> c) -> (a -> f c)
ff >$> g = fmap g . ff

globalCfg :: ZWaveHome -> [DeviceId] -> ZWave Moment (Event (IO ()))
globalCfg home ds = dimmerCfg home ds ds toLevel
  where
    toLevel :: Scene -> Maybe Integer
    toLevel TripleDown = Just 0
    toLevel _          = Nothing

multiDimmerCfg :: ZWaveHome -> [DeviceId] -> ZWave Moment (Event (IO ()))
multiDimmerCfg home ds = dimmerCfg home ds ds $ toLevel >>> Just
  where
    toLevel :: Scene -> Integer
    toLevel DoubleUp   = 0xFF
    toLevel DoubleDown = 0
    toLevel TripleUp   = 0x63
    toLevel TripleDown = 0

showHomeEventDiff :: MonadMoment m => Event DeviceMap -> m (Event String)
showHomeEventDiff e = do
    b <- stepper Map.empty e
    let diff
            :: DeviceMap
            -> DeviceMap
            -> Map
                   DeviceId
                   ( Change
                         Device
                         ( Map
                               ValueId
                               (Change Value (ValueState, ValueState))
                         )
                   )
        diff = mergeMaps
            (\Device { _deviceValues = a } Device { _deviceValues = b } ->
                mergeMaps
                    (\Value { _valueState = a } Value { _valueState = b } ->
                        (a, b)
                    )
                    a
                    b
            )
        showDiff map =
            List.unlines
                $   Map.toList map
                <&> (\(d, c) -> show d ++ ": " ++ showChange
                        c
                        (\map' ->
                            List.unlines
                                $   Map.toList map'
                                <&> (\(v, c') -> show v ++ ": " ++ showChange
                                        c'
                                        (\(a, b) -> show a ++ " => " ++ show b)
                                    )
                        )
                    )
        showChange c f = go c
          where
            go (Added a)   = "added"
            go Deleted     = "deleted"
            go (Changed b) = f b

    return . fmap showDiff . filterE (not . Map.null) $ diff <$> b <@> e

mergeE :: (b -> b -> b) -> [Event b] -> Event b
mergeE f = List.foldl' (unionWith f) never

getDoorEvent :: ZWaveDevice -> ZWave' (Event DoorState)
getDoorEvent d =
    getDeviceValueByName "Access Control" d
        <&> (   valueChanges
            >>> fmap (preview _VByte)
            >>> filterJust
            >>> fmap lookup
            >>> filterJust
            )
  where
    lookup 22 = Just Open
    lookup 23 = Just Closed
    lookup _  = Nothing

getSceneEvent :: ZWaveDevice -> ZWave' (Event Scene)
getSceneEvent d =
    getDeviceValueByName "Scene Number" d
        <&> (   valueChanges
            >>> fmap (preview _VByte)
            >>> filterJust
            >>> fmap (flip Map.lookup sceneNumberMap)
            >>> filterJust
            )
  where
    sceneNumberMap :: Map Integer Scene
    sceneNumberMap = Map.fromList
        [(13, DoubleUp), (14, TripleUp), (23, DoubleDown), (24, TripleDown)]

singleDimmerCfg :: ZWaveHome -> DeviceId -> ZWave Moment (Event (IO ()))
singleDimmerCfg home d = do
    device   <- getDeviceById d home
    levelV   <- getDeviceValueByName "Level" device
    sceneEvt <- getSceneEvent device

    let handlerB :: Behavior (Scene -> Maybe Integer)
        handlerB =
            getValue levelV
                <&> ((>>= (preview _VByte >>> fmap handleScene)) >>> liftM)
        newLevelEvt = filterJust $ handlerB <@> sceneEvt

    setValueByteOnEvt levelV newLevelEvt
  where
    handleScene :: Integer -> Scene -> Maybe Integer
    handleScene 0 DoubleUp   = Just 0xFF -- preview level
    handleScene 0 TripleUp   = Just 0xFF
    handleScene _ DoubleUp   = Just 0x63 -- max level
    handleScene _ TripleUp   = Just 0x63 -- max level
    handleScene _ DoubleDown = Just 0  -- off
    handleScene _ TripleDown = Just 0

    liftM ff a = ($ a) =<< ff

washerCfg
    :: [SMTP.Address] -> ZWaveHome -> DeviceId -> ZWave Moment (Event (IO ()))
washerCfg addrs home d = do
    device <- getDeviceById d home
    powerV <- getDeviceValueByName "Power" device
    let powerE = (^?! _VDecimal) <$> valueChanges powerV
    powerB <- stepper 0 powerE

    let reactToChange :: Float -> Float -> IO ()
        reactToChange old new = when (old > 0 && new < 0.5) $ sendEmail addrs

    return $ (powerB <&> reactToChange) <@> powerE

sendEmail :: [SMTP.Address] -> IO ()
sendEmail to = SMTP.sendMail "localhost" mail
  where
    mail = SMTP.simpleMail from to [] [] subject [SMTP.plainTextPart msg]
    from =
        SMTP.Address (Just "Washing Machine") "washer@melrose.steellworks.com"
    subject = "The washing machine has finished"
    msg     = "You may now move your clothes to the dryer"
