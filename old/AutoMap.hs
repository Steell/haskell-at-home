{-# LANGUAGE RecordWildCards #-}

module AutoMap where

import           Data.Bimap                    as BM

data AutoMap a = AutoMap { _map :: !(Bimap Int a)
                         , _nextIdx :: !Int
                         }

empty :: AutoMap a
empty = AutoMap BM.empty 0

insert :: Ord a => a -> AutoMap a -> (Int, AutoMap a)
insert a AutoMap {..} =
    (_nextIdx, AutoMap (BM.insert _nextIdx a _map) (_nextIdx + 1))

delete :: Ord a => Int -> AutoMap a -> AutoMap a
delete k map@AutoMap {..} = map { _map = BM.delete k _map }

deleteR :: Ord a => a -> AutoMap a -> AutoMap a
deleteR a map@AutoMap {..} = map { _map = BM.deleteR a _map }

lookup :: Ord a => Int -> AutoMap a -> Maybe a
lookup k = BM.lookup k . _map

lookupR :: Ord a => a -> AutoMap a -> Maybe Int
lookupR a = BM.lookupR a . _map
