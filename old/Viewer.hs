module Viewer where

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
