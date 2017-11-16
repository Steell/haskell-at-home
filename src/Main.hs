{-# language OverloadedStrings #-}

module Main where

import Brick

import Control.Comonad

import Data.Monoid ((<>))
import Data.Text (Text, pack)

import qualified Graphics.Vty as V
import Graphics.Vty.Attributes (defAttr)
import Graphics.Vty.Input.Events

import Data.RoseTree
import Data.RoseTree.Zipper
import Widgets.Tree

---

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

data AppState n = AppState
  { _deviceTree :: Tree n Device
  }

smarthomeApp :: App (AppState Text) e Text
smarthomeApp =
  App
  { appDraw = draw
  , appChooseCursor = cursor
  , appHandleEvent = event
  , appStartEvent = startEvent
  , appAttrMap = attrs
  }
  where
    draw AppState {..} = [renderTree renderEntry True _deviceTree]
    renderEntry _ z =
      vLimit 1 $ txt (label z) <+> fill ' '
    label z = pack (replicate (generation z) ' ') <> (_deviceName . snd $ extract z)
    cursor _ (x:_) = Just x
    cursor _ _ = Nothing
    event s (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt s
    event s@AppState {..} (VtyEvent e) = do
      l <- handleTreeEvent e _deviceTree
      continue $ s {_deviceTree = l}
    event s _ = continue s
    startEvent = return
    attrs _ =
      attrMap defAttr [(treeSelectedAttr, defAttr `V.withStyle` V.reverseVideo)]

index :: Zipper (Bool, e) -> Int
index z@Zipper{_context = (ls, _, _) : _,..} =
    1 + sum (fmap (length . viewTree) ls) + index (up z)
index _ = 0

viewTree :: RoseTree (Bool, e) -> [Zipper (Bool, e)]
viewTree a = go (fromRoseTree a)
  where
    collapsed z = not . fst $ extract z
    go z | collapsed z = [ z ]
         | otherwise = z : maybe [] descent (downM z)
    descent z = go z <> maybe [] descent (rightM z)

main :: IO ()
main = do
  _ <- defaultMain smarthomeApp ashView
  return ()
  where
    convertH :: Home [] -> DeviceTree
    convertH Home {..} = RoseTree (fmap convertR _rooms) (Device _homeName)
    convertR :: Room [] -> DeviceTree
    convertR Room {..} =
      RoseTree (fmap convertD _devices) (Device _roomName)
    convertD :: Device -> DeviceTree
    convertD d = RoseTree [] d
    ashView :: AppState Text
    ashView = AppState {_deviceTree = tree "deviceTree" (convertH ash) 1}


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
