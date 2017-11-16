module Data.RoseTree where

data RoseTree n = RoseTree [RoseTree n] n

children :: RoseTree n -> [RoseTree n]
children (RoseTree cs _) = cs

node :: RoseTree n -> n
node (RoseTree _ n) = n

instance Functor RoseTree where
  fmap f (RoseTree cs n) = RoseTree (fmap (fmap f) cs) (f n)
