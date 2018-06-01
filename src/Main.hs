{-# LANGUAGE TupleSections #-}
{-# language OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow
import Control.Lens
import Control.Monad
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time.Clock
import Data.Time.Horizon
import Foreign.C.Types hiding (CBool)
import Reactive.Banana.Combinators

import ReactiveDaemon
import ReactiveDaemon.OpenZWave

{-
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Input.Events
import System.Daemon
import System.Environment ( getArgs )
import System.IO.Error
import Widgets.Tree (Tree)
import qualified Widgets.Tree as T
import Pipes
import qualified Graphics.Vty as V
import Data.Monoid ((<>))
import Data.RoseTree
import Data.RoseTree.Zipper
import Data.String ( fromString )
import Data.Text (Text, pack)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Control.Pipe.Serialize ( serializer, deserializer )
import Daemon
import Data.ByteString (ByteString)
import Data.Char ( toLower )
import Data.Default (def)
import Brick
import Brick.BChan
import qualified Brick.Widgets.Border as B
import Brick.Widgets.List (List)
import qualified Brick.Widgets.List as L
import Control.Comonad
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
-}

---

main :: IO ()
main = dMain myconfig

type ZWave' = ZWave Moment

data DoorState = Open | Closed

myconfig :: ZWave' (Event (IO ()))
myconfig = List.foldl' (unionWith (>>)) never <$> sequenceA [ singleDimmerCfg guestroom
                                                            , singleDimmerCfg diningroom
                                                            , multiDimmerCfg bedroom
                                                            , multiDimmerCfg livingroom
                                                            , globalCfg all
							    , entranceCfg frontDoorSensor [livingroomEntry]
                                                            ]
  where
    livingroom = [ livingroomEntry, livingroomMantle, livingroomSeating ]
    livingroomEntry = 3
    livingroomMantle = 4
    livingroomSeating = 5
    guestroom = 2
    bedroom = [ bedroomFront, bedroomKellie, bedroomSteve ]
    bedroomFront = 6
    bedroomKellie = 7
    bedroomSteve = 8
    diningroom = 9
    all = [3 .. 9]
    washerOutlet = 10
    frontDoorSensor = 11

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

dimmerCfg :: [NodeId] -> [NodeId] -> (Scene -> Maybe CUChar) -> ZWave Moment (Event (IO ()))
dimmerCfg ins outs toLevel = do
    inDevs <- mapM getDeviceById ins
    outDevs <- mapM getDeviceById outs

    let setLevelsOnEvt byte = List.foldl (unionWith (>>)) never $
            flip setValueByteOnEvt byte .
                getDeviceValueByName "Level" <$>
                outDevs
        newLevelEvt = mergeE $
            inDevs <&> (getSceneEvent >>> fmap toLevel >>> filterJust)

    return $ setLevelsOnEvt newLevelEvt

globalCfg :: [NodeId] -> ZWave Moment (Event (IO ()))
globalCfg ds = dimmerCfg ds ds toLevel
  where
    toLevel :: Scene -> Maybe CUChar
    toLevel TripleDown = Just 0
    toLevel _ = Nothing

multiDimmerCfg :: [NodeId] -> ZWave Moment (Event (IO ()))
multiDimmerCfg ds = dimmerCfg ds ds $ toLevel >>> Just
  where
    toLevel :: Scene -> CUChar
    toLevel DoubleUp = 0xFF
    toLevel DoubleDown = 0
    toLevel TripleUp = 0x63
    toLevel TripleDown = 0

mergeE :: [Event b] -> Event b
mergeE = List.foldl' (unionWith const) never

getDoorEvent :: ZWaveDevice -> Event DoorState
getDoorEvent = getDeviceValueByName "Access Control" >>>
    valueChanges >>>
        fmap (preview _VTByte) >>>
	    filterJust >>>
	        fmap lookup >>>
		    filterJust
  where lookup 22 = Just Open
        lookup 23 = Just Closed
	lookup _ = Nothing

getSceneEvent :: ZWaveDevice -> Event Scene
getSceneEvent = getDeviceValueByName "Scene Number" >>>
    valueChanges >>>
        fmap (preview _VTByte) >>>
            filterJust >>>
                fmap (flip Map.lookup sceneNumberMap) >>>
                    filterJust
  where
    sceneNumberMap :: Map CUChar Scene
    sceneNumberMap = Map.fromList [ (13, DoubleUp)
                                  , (14, TripleUp)
                                  , (23, DoubleDown)
                                  , (24, TripleDown)
                                  ]

singleDimmerCfg :: NodeId -> ZWave Moment (Event (IO ()))
singleDimmerCfg d = do
    device <- getDeviceById d

    let sceneEvt = getSceneEvent device
        levelV = getDeviceValueByName "Level" device
        handlerB :: Behavior (Scene -> Maybe CUChar)
        handlerB = getValue levelV <&>
            ((>>= ((preview _VTByte) >>> fmap handleScene)) >>> liftM)
        newLevelEvt = filterJust $ handlerB <@> sceneEvt

    return $ setValueByteOnEvt levelV newLevelEvt
  where
    handleScene :: CUChar -> Scene -> Maybe CUChar
    handleScene 0 DoubleUp =
        Just 0xFF -- preview level
    handleScene 0 TripleUp =
        Just 0xFF
    handleScene _ DoubleUp =
        Just 0x63 -- max level
    handleScene _ TripleUp =
        Just 0x63 -- max level
    handleScene _ DoubleDown =
        Just 0  -- off
    handleScene _ TripleDown =
        Just 0

    liftM ff a = join $ fmap ($ a) ff

isSunOut :: Monad m => ZWave m (Behavior Bool)
isSunOut = currentTimeB <&> fmap (\ (UTCTime day time) ->
    let (UTCTime _ morning) = sunrise day 42.458429 (-71.066163)
        (UTCTime _ evening) = sunset day 42.458429 (-71.066163)
    in
        time >= morning && time <= evening)

{-
data Home l = Home
  { _homeName :: Text
  , _rooms :: l (Room l)
  }
data Room l = Room
  { _roomName :: Text
  , _devices :: l Device
  }
data Device = Device { _deviceName :: Text }

type DeviceTree = RoseTree Device

data AppState n = Overview (DeviceView n)
                | AddDevice (List n Device) (DeviceView n)
                  -- TODO: I think openzwave just waits for the inclusion signal.
                  --       It doesn't send any more notifications.
                  --       We should just show a message and wait for:
                  --         a. user cancel (C-c) -> CancelControllerCommand
                  --         b. node added event

data DeviceView n = TreeView (Tree n Device)
                  | ListView (List n Device)

data ZWaveEvent

overviewUi devices = [ ui ]
  where
    ui = vBox [ box, commands ]
    box = B.borderWithLabel (txt "Devices") devices
    commands = txt "C-n : add device  |  C-v : change view  |  C-c : quit"

addDeviceUi devices = [ ui ]
  where
    ui = vBox [box, commands]
    box = B.borderWithLabel (txt "Detecting devices") devices
    commands = txt "C-c : cancel"

draw :: (Ord n, Show n) => AppState n -> [Widget n]
draw (Overview (TRDreeView _deviceTree)) = overviewUi devices
  where
    devices = T.renderTree renderEntry True _deviceTree
    renderEntry _ z = vLimit 1 $ txt (label z) <+> fill ' '
    label z = pack (replicate (generation z) ' ') <>
        (_deviceName . snd $ extract z)

draw (Overview (ListView deviceList)) = overviewUi devices
  where
    devices = L.renderList renderEntry True deviceList
    renderEntry _ d = vLimit 1 $ txt (_deviceName d) <+> fill ' '

draw (AddDevice deviceList _) = addDeviceUi devices
  where
    devices = L.renderList renderEntry True deviceList
    renderEntry _ d = vLimit 1 $ txt (_deviceName d) <+> fill ' '

smarthomeApp :: App (AppState Text) ZWaveEvent Text
smarthomeApp =
  App
  { appDraw = draw
  , appChooseCursor = cursor
  , appHandleEvent = event
  , appStartEvent = startEvent
  , appAttrMap = attrs
  }
  where
    cursor _ (x:_) = Just x
    cursor _ _ = Nothing
    event s@(Overview _) (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt s

    event (Overview v) (VtyEvent (EvKey (KChar 'n') [MCtrl])) =
      continue $ AddDevice (L.list "add-device-list" Vector.empty 1) v

    event (Overview v) (VtyEvent (EvKey (KChar 'v') [MCtrl])) =
      continue . Overview $ case v of ListView l -> TreeView (toTree l)
                                      TreeView t -> ListView (toList t)
      where
        toTree l = undefined --TODO
        toList t = undefined --TODO

    event (Overview (ListView l)) (VtyEvent e) = do
      l' <- L.handleListEvent e l
      continue $ Overview (ListView l')

    event (Overview (TreeView t)) (VtyEvent e) = do
      t' <- T.handleTreeEvent e t
      continue $ Overview (TreeView t')

    event s _ = continue s
    startEvent = return
    attrs _ =
      attrMap defAttr [(T.treeSelectedAttr, defAttr `V.withStyle` V.reverseVideo)]
-}

{-
    evtChan <- newBChan 100
    _ <- customMain (V.mkVty V.defaultConfig)
                    (Just evtChan)
                    smarthomeApp
                    (Overview . ListView $ L.list "device-list" Vector.empty 1)
-}
{-
  where
    ashTree = convertH ash
    ashView = Overview . TreeView $ T.tree "deviceTree" ashTree 1

convertH :: Home [] -> DeviceTree
convertH Home {..} = RoseTree (fmap convertR _rooms) (Device _homeName)
convertR :: Room [] -> DeviceTree
convertR Room {..} =
  RoseTree (fmap convertD _devices) (Device _roomName)
convertD :: Device -> DeviceTree
convertD = RoseTree []

ash :: Home []
ash =
  Home
  { _homeName = "22 Ash"
  , _rooms =
      [ masterBedroom
      , guestRoom
      , upperHall
      , upperBath
      , livingRoom
      , diningRoom
      , kitchen
      , basementDen
      , basementBath
      , basementUtility
      ]
  }
  where
    masterBedroom =
      Room
      { _roomName = "Master Bedroom"
      , _devices =
          [ Device {_deviceName = "Lamp"}
          , Device {_deviceName = "Dresser Lights"}
          , Device {_deviceName = "Kellie Overhead Light"}
          , Device {_deviceName = "Steve Overhead Light"}
          , Device {_deviceName = "Kellie Closet Light"}
          , Device {_deviceName = "Steve Closet Light"}
          ]
      }
    guestRoom =
      Room
      { _roomName = "Guest Bedroom"
      , _devices =
          [ Device {_deviceName = "Overhead Lights"}
          , Device {_deviceName = "Lamp"}
          , Device {_deviceName = "Stereo"}
          , Device {_deviceName = "Chromecast Audio"}
          ]
      }
    upperHall =
      Room
      { _roomName = "Upstairs Hallway"
      , _devices = [Device {_deviceName = "Nest Protect"}]
      }
    upperBath = Room {_roomName = "Upstairs Bathroom", _devices = []}
    livingRoom =
      Room
      { _roomName = "Living Room"
      , _devices =
          [ Device {_deviceName = "Seating Lights"}
          , Device {_deviceName = "Mantle Lights"}
          , Device {_deviceName = "Walkway Lights"}
          , Device {_deviceName = "Lamp"}
          , Device {_deviceName = "Nest Cam"}
          , Device {_deviceName = "Nest Protect"}
          , Device {_deviceName = "Chromecast Ultra"}
          , Device {_deviceName = "Chromecast Audio"}
          , Device {_deviceName = "Google Home"}
          ]
      }
    diningRoom =
      Room
      { _roomName = "Dining Room"
      , _devices =
          [ Device {_deviceName = "Nest Thermostat E"}
          , Device {_deviceName = "Overhead Light"}
          , Device {_deviceName = "Google Home Mini"}
          , Device {_deviceName = "Chomecast Audio"}
          ]
      }
    kitchen =
      Room
      { _roomName = "Kitchen"
      , _devices =
          [ Device {_deviceName = "Google Home"}
          , Device {_deviceName = "Nest Cam"}
          ]
      }
    basementDen =
      Room
      { _roomName = "Den"
      , _devices =
          [ Device {_deviceName = "Rear Left Overhead Light"}
          , Device {_deviceName = "Rear Right Overhead Light"}
          , Device {_deviceName = "Front Left Overhead Light"}
          , Device {_deviceName = "Front Right Overhead Light"}
          , Device {_deviceName = "Google Home"}
          ]
      }
    basementBath =
      Room
      { _roomName = "Basement Bathroom"
      , _devices =
          [ Device {_deviceName = "LG Washer"}
          , Device {_deviceName = "LG Dryer"}
          ]
      }
    basementUtility = Room {_roomName = "Utility Room", _devices = []}
-}
