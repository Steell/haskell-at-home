{-# language OverloadedStrings #-}
module UI where

import Control.Lens
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
import ReactiveDaemon
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

data Home l = Home
  { _homeName :: Text
  , _rooms :: l (Room l)
  }
data Room l = Room
  { _roomName :: Text
  , _devices :: l Device
  }
data Device = Device { _dName :: Text }

type DeviceTree = RoseTree Device

data AppState n = Overview (DeviceView n)

data DeviceView n = TreeView (Tree n Device)
                  | ListView (List n Device)

overviewUi :: Widget n -> [Widget n]
overviewUi devices = [ ui ]
  where
    ui = vBox [ box, commands ]
    box = B.borderWithLabel (txt "Devices") devices
    commands = txt "C-n : add device  |  C-v : change view  |  C-c : quit"

draw :: (Ord n, Show n) => AppState n -> [Widget n]
draw (Overview (TreeView _deviceTree)) = overviewUi devices
  where
    devices = T.renderTree renderEntry True _deviceTree
    renderEntry _ z = vLimit 1 $ txt (label z) <+> fill ' '
    label z = pack (replicate (generation z) ' ') <>
        (_dName . snd $ extract z)

draw (Overview (ListView deviceList)) = overviewUi devices
  where
    devices = L.renderList renderEntry True deviceList
    renderEntry _ d = vLimit 1 $ txt (_dName d) <+> fill ' '

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

    event (Overview v) (AppEvent (DeviceList dmap)) =
        continue . Overview $ updateView v dmap
      where
        updateView (ListView l) = ListView . updateList l
        updateView (TreeView t) = TreeView . updateTree t
        updateTree t dmap =
          let selected = t^.T.treeSelectedL
          in
            undefined
        updateList l dmap = undefined

    event s _ = continue s
    startEvent = return
    attrs _ =
      attrMap defAttr [(T.treeSelectedAttr, defAttr `V.withStyle` V.reverseVideo)]
