module Client where

-----------------------------

type ZWave' = ZWave Moment

data DoorState = Open | Closed

myconfig :: ZWave' (Event (IO ()))
myconfig = List.foldl' (unionWith (>>)) never <$> sequenceA
    [ singleDimmerCfg guestroom
    , singleDimmerCfg diningroom
    , multiDimmerCfg bedroom
    , multiDimmerCfg livingroom
    , globalCfg all
    -- , entranceCfg frontDoorSensor [livingroomEntry]
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

{-
entranceCfg :: NodeId -> [NodeId] -> ZWave Moment (Event (IO ()))
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
    :: [NodeId]
    -> [NodeId]
    -> (Scene -> Maybe CUChar)
    -> ZWave Moment (Event (IO ()))
dimmerCfg ins outs toLevel = do
    inDevs  <- mapM getDeviceById ins
    outDevs <- mapM getDeviceById outs

    let setLevelsOnEvt byte =
            List.foldl (unionWith (>>)) never
                $   flip setValueByteOnEvt byte
                .   getDeviceValueByName "Level"
                <$> outDevs
        newLevelEvt =
            mergeE $ inDevs <&> (getSceneEvent >>> fmap toLevel >>> filterJust)

    return $ setLevelsOnEvt newLevelEvt

globalCfg :: [NodeId] -> ZWave Moment (Event (IO ()))
globalCfg ds = dimmerCfg ds ds toLevel
  where
    toLevel :: Scene -> Maybe CUChar
    toLevel TripleDown = Just 0
    toLevel _          = Nothing

multiDimmerCfg :: [NodeId] -> ZWave Moment (Event (IO ()))
multiDimmerCfg ds = dimmerCfg ds ds $ toLevel >>> Just
  where
    toLevel :: Scene -> CUChar
    toLevel DoubleUp   = 0xFF
    toLevel DoubleDown = 0
    toLevel TripleUp   = 0x63
    toLevel TripleDown = 0

mergeE :: [Event b] -> Event b
mergeE = List.foldl' (unionWith const) never

getDoorEvent :: ZWaveDevice -> Event DoorState
getDoorEvent =
    getDeviceValueByName "Access Control"
        >>> valueChanges
        >>> fmap (preview _VTByte)
        >>> filterJust
        >>> fmap lookup
        >>> filterJust
  where
    lookup 22 = Just Open
    lookup 23 = Just Closed
    lookup _  = Nothing

getSceneEvent :: ZWaveDevice -> Event Scene
getSceneEvent =
    getDeviceValueByName "Scene Number"
        >>> valueChanges
        >>> fmap (preview _VTByte)
        >>> filterJust
        >>> fmap (flip Map.lookup sceneNumberMap)
        >>> filterJust
  where
    sceneNumberMap :: Map CUChar Scene
    sceneNumberMap = Map.fromList
        [(13, DoubleUp), (14, TripleUp), (23, DoubleDown), (24, TripleDown)]

singleDimmerCfg :: NodeId -> ZWave Moment (Event (IO ()))
singleDimmerCfg d = do
    device <- getDeviceById d

    let sceneEvt = getSceneEvent device
        levelV   = getDeviceValueByName "Level" device
        handlerB :: Behavior (Scene -> Maybe CUChar)
        handlerB =
            getValue levelV
                <&> ((>>= (preview _VTByte >>> fmap handleScene)) >>> fmap)
        newLevelEvt = filterJust $ handlerB <@> sceneEvt

    return $ setValueByteOnEvt levelV newLevelEvt
  where
    handleScene :: CUChar -> Scene -> Maybe CUChar
    handleScene 0 DoubleUp   = Just 0xFF -- preview level
    handleScene 0 TripleUp   = Just 0xFF
    handleScene _ DoubleUp   = Just 0x63 -- max level
    handleScene _ TripleUp   = Just 0x63 -- max level
    handleScene _ DoubleDown = Just 0  -- off
    handleScene _ TripleDown = Just 0

    liftM ff a = ($ a) =<< ff