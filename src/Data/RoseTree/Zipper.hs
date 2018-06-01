module Data.RoseTree.Zipper where

import Control.Comonad

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.RoseTree

type BreadCrumb n = ([RoseTree n], n, [RoseTree n])

data Zipper n = Zipper
  { _tree :: RoseTree n
  , _context :: [BreadCrumb n]
  }

instance Functor Zipper where
  fmap f Zipper {..} = Zipper (fmap f _tree) (fmap mapCrumb _context)
    where
      mapCrumb (ls, n, rs) = (fmap (fmap f) ls, f n, fmap (fmap f) rs)

instance Comonad Zipper where
  extract Zipper {_tree = RoseTree _ n} = n
  duplicate z = Zipper (dupedTree z) (dupedContext z)
    where
      dupedTree z =
        RoseTree (maybe [] (fmap dupedTree . iterateM rightM) (downM z)) z
      dupedCrumbs z =
        (maybe [] dupedLefts (leftM z), z, maybe [] dupedRights (rightM z)) :
        dupedContext z
      dupedContext z = maybe [] dupedCrumbs $ upM z
      dupedLefts = fmap dupedTree . iterateM leftM
      dupedRights = fmap dupedTree . iterateM rightM

iterateM :: (a -> Maybe a) -> a -> [a]
iterateM f x =
  x :
  case f x of
    Nothing -> []
    Just y -> iterateM f y

upM, downM, leftM, rightM :: Zipper n -> Maybe (Zipper n)
rightM Zipper {_context = (ls, n, r:rs):as, ..} =
  Just Zipper {_tree = r, _context = (_tree : ls, n, rs) : as}
rightM _ = Nothing

leftM Zipper {_context = (l:ls, n, rs):as, ..} =
  Just Zipper {_tree = l, _context = (ls, n, _tree:rs) : as}
leftM _ = Nothing

downM Zipper {_tree = RoseTree (c:cs) n, ..} =
  Just Zipper {_tree = c, _context = ([], n, cs) : _context}
downM _ = Nothing

upM Zipper {_context = (ls, n, rs):as, ..} =
  Just Zipper {_tree = RoseTree (reverse ls <> (_tree : rs)) n, _context = as}
upM _ = Nothing

up, down, left, right :: Zipper n -> Zipper n
up z = fromMaybe z $ upM z
down z = fromMaybe z $ downM z
left z = fromMaybe z $ leftM z
right z = fromMaybe z $ rightM z

generation :: Zipper n -> Int
generation = length . _context

toRoseTree :: Zipper n -> RoseTree n
toRoseTree z = case upM z of Nothing -> _tree z
                             Just z -> toRoseTree z

fromRoseTree :: RoseTree n -> Zipper n
fromRoseTree tree = Zipper tree []

modifyData :: (n -> n) -> Zipper n -> Zipper n
modifyData f z@Zipper {_tree = RoseTree cs n} = z {_tree = RoseTree cs (f n)}

insert :: n -> Zipper n -> Zipper n
insert = modifyData . const
