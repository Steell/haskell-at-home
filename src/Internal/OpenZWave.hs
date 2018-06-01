{-# language LambdaCase, TupleSections #-}
module Internal.OpenZWave where

import Control.Arrow
import Control.Applicative

import Data.Functor
import Data.Word

import OpenZWave.Ozw
import OpenZWave.Std

import Reactive.Banana
import Reactive.Banana.Frameworks

type NodeId = Word8

data DimmerButton = Up | Down
data ButtonEvent = SinglePress | DoublePress | TriplePress | Hold | Release

data Dimmer = Dimmer { _setPowered :: Bool -> IO ()
                     , _buttonEvent :: Event (DimmerButton, ButtonEvent)
                     }

(&) = flip ($)

dimmer :: NodeId -> ValueID -> Event Notification -> Dimmer
dimmer n v = error "Implement function: dimmer"

bedroom :: Event Notification -> MomentIO ()
bedroom notifE = do
    let frontLightsDimmer = dimmer undefined undefined notifE
        steveLightDimmer = dimmer undefined undefined notifE
        kellieLightDimmer = dimmer undefined undefined notifE

        dimmerGroup = [ frontLightsDimmer, steveLightDimmer, kellieLightDimmer ]

        powerE = (_buttonEvent <$> dimmerGroup) &
            foldr1 (unionWith const) &
            filterE isDoublePress &
            fmap powerButton

        powerExE = ((\(d, ds) -> (,ds) <$> _buttonEvent d) <$> extract dimmerGroup) &
            foldr1 (unionWith const) &
            filterE (\(e, _) -> isTriplePress e) &
            fmap (\(e, ds) -> (powerButton e, ds))

    reactimate $
        fmap (\b -> mapM_ (flip _setPowered b) dimmerGroup) powerE
    reactimate $
        fmap (\(b, ds) -> mapM_ (flip _setPowered b) ds) powerExE
  where
    isDoublePress (_, DoublePress) =
        True
    isDoublePress _ = False

    isTriplePress (_, TriplePress) =
        True
    isTriplePress _ = False

    powerButton (Up, _) = True
    powerButton _ = False


extract :: [a] -> [(a, [a])]
extract = go []
  where
    go _ [] = []
    go ls (x:rs) = (x, reverse ls ++ rs) : go (x:ls) rs
