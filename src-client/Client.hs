{-# LANGUAGE TypeOperators #-}

module Client (
    runClient
    , Change(..)
    , mergeMaps
    )
where

import           Api                            ( ConduitClient
                                                , clientIO
                                                , handleState
                                                , handleEvents
                                                )


import           Control.Concurrent.Async       ( race_ )
import           Control.Monad                  ( void )

import qualified Data.Conduit.Combinators      as Conduit
import qualified Data.Map.Merge.Strict         as Map
import           Data.Map.Strict                ( Map )

import           Reactive.Banana
import           Reactive.Banana.Frameworks

import           ReactiveDaemon

import           Servant.Client                 ( ClientEnv )

runClient :: ClientEnv -> ZWave Moment (Event (IO ())) -> IO ()
runClient cenv cfg = do
    (eventHandler, writeEvent) <- newAddHandler
    (stateHandler, writeState) <- newAddHandler
    let client  = clientIO cenv
        netDesc = dNetwork client cfg stateHandler eventHandler
    compile netDesc >>= actuate
    race_ (thread (handleState client) writeState)
          (thread (handleEvents client) writeEvent)
  where
    thread :: (ConduitClient a () -> IO ()) -> (a -> IO ()) -> IO ()
    thread connect write = void . connect $ do
        liftIO $ putStrLn "Connected to socket."
        Conduit.mapM_ (liftIO . write)

data Change a b = Added a | Deleted | Changed b

mergeMaps
    :: (Eq a, Ord k)
    => (a -> a -> b)
    -> Map k a
    -> Map k a
    -> Map k (Change a b)
mergeMaps f = Map.merge deleted added matched
  where
    matched = Map.zipWithMaybeMatched . const $ \a b ->
        if a == b then Nothing else Just (Changed $ f a b)
    deleted = Map.mapMissing . const $ const Deleted
    added   = Map.mapMissing $ const Added
