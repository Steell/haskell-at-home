module Data.RoseTree where

data RoseTree n = RoseTree [RoseTree n] n

children :: RoseTree n -> [RoseTree n]
children (RoseTree cs _) = cs

node :: RoseTree n -> n
node (RoseTree _ n) = n

instance Functor RoseTree where
  fmap f (RoseTree cs n) = RoseTree (fmap (fmap f) cs) (f n)

foldTree :: (a -> [b] -> b) -> RoseTree a -> b
foldTree f = go
  where go (RoseTree cs n) = f n $ fmap go cs
