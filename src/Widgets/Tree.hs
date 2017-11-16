{-# LANGUAGE TupleSections #-}
module Widgets.Tree
    ( Tree
    , tree
    , renderTree
    , handleTreeEvent
    , treeSelectedAttr
    ) where

import Brick
import Brick.Widgets.List

import Control.Comonad

import Data.Monoid ((<>))
import Data.RoseTree
import Data.RoseTree.Zipper (Zipper(..))
import qualified Data.RoseTree.Zipper as Zipper
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Graphics.Vty.Input.Events

import Lens.Micro ((.~), (&))

newtype Tree n e = Tree { _unTree :: List n (Zipper (Bool, e)) }

treeSelectedAttr :: AttrName
treeSelectedAttr = listSelectedAttr

tree :: n -> RoseTree e -> Int -> Tree n e
tree n t h = Tree $
    list n (Vector.singleton . Zipper.fromRoseTree $ fmap (False,) t) h

renderTree :: (Show n, Ord n)
           => (Bool -> Zipper (Bool, e) -> Widget n)
           -> Bool
           -> Tree n e
           -> Widget n
renderTree f b (Tree l) = renderList f b l

handleTreeEvent :: Ord n => Event -> Tree n e -> EventM n (Tree n e)
handleTreeEvent (EvKey KLeft _) t = return $ collapse t
handleTreeEvent (EvKey KRight _) t = return $ expand t
handleTreeEvent e (Tree l) = Tree <$> handleListEvent e l

collapse :: Tree n e -> Tree n e
collapse (Tree l) = Tree $
    case listSelectedElement l of
        Nothing -> l
        Just (_, z) -> case extract z of
            (True, _) -> l & listElementsL .~
                (Vector.fromList .
                     viewTree . Zipper.toRoseTree $
                     toggle z)
            (False, _) -> let z' = toggle $ Zipper.up z
                          in
                              listReplace (viewZipper z') (Just $ index z') l

expand :: Tree n e -> Tree n e
expand (Tree l) = Tree $
    case listSelectedElement l of
        Nothing -> l
        Just (_, z)
            | null . children $ Zipper._tree z -> l
            | otherwise -> case extract z of
                  (False, _) -> let z' = toggle z
                                in
                                    listReplace (viewZipper z')
                                                (Just . index $ Zipper.down z')
                                                l
                  (True, _) -> l

index :: Zipper (Bool, e) -> Int
index z@Zipper{_context = (ls, _, _) : _,..} =
    1 + sum (fmap (length . viewTree) ls) + index (Zipper.up z)
index _ = 0

toggle :: Zipper (Bool, b) -> Zipper (Bool, b)
toggle = Zipper.modifyData (\(b, t) -> (not b, t))

viewZipper :: Zipper (Bool, e) -> Vector (Zipper (Bool, e))
viewZipper = Vector.fromList . viewTree . Zipper.toRoseTree

viewTree :: RoseTree (Bool, e) -> [Zipper (Bool, e)]
viewTree a = go (Zipper.fromRoseTree a)
  where
    collapsed z = not . fst $ extract z
    go z | collapsed z = [ z ]
         | otherwise = z : maybe [] descent (Zipper.downM z)
    descent z = go z <> maybe [] descent (Zipper.rightM z)
