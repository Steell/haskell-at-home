module Data.List.Zipper where

import Control.Comonad
import Data.Monoid ((<>))

data Zipper a = Zipper [a] a [a]

instance Functor Zipper where
  fmap f (Zipper l a r) = Zipper (fmap f l) (f a) (fmap f r)

instance Comonad Zipper where
  extract (Zipper _ x _) = x
  duplicate z = Zipper (tail $ iterate left z) z (tail $ iterate right z)

right :: Zipper a -> Zipper a
right (Zipper ls x (r:rs)) = Zipper (x:ls) r rs
right z = z

left :: Zipper a -> Zipper a
left (Zipper (l:ls) x rs) = Zipper ls l (x:rs)
left z = z

advance :: Zipper a -> Maybe (Zipper a)
advance (Zipper ls x (r:rs)) = Just (Zipper (x:ls) r rs)
advance _ = Nothing

retreat :: Zipper a -> Maybe (Zipper a)
retreat (Zipper (l:ls) x rs) = Just $ Zipper ls l (x:rs)
retreat _ = Nothing

toList :: Zipper a -> [a]
toList (Zipper ls x rs) = reverse ls <> (x : rs)

fromList :: [a] -> Maybe (Zipper a)
fromList [] = Nothing
fromList (a:as) = Just $ Zipper [] a as
