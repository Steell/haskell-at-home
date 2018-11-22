{-# LANGUAGE TypeOperators #-}

module Client2
    ( runClient
    , Change(..)
    , mergeMaps
    )
where

import           Api                            ( ConduitClient
                                                , API
                                                , Device(..)
                                                , DeviceId
                                                , Home(..)
                                                , HomeId
                                                , HomeMap
                                                , Value(..)
                                                , ValueId
                                                , ValueState
                                                , clientIO
                                                , handleState
                                                )

import           Conduit                        ( await
                                                , (.|)
                                                )

import           Control.Concurrent.Async       ( async )
import           Control.Monad                  ( void )
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                , asks
                                                )

import           Data.Conduit                   ( ConduitT )
import qualified Data.Conduit.Combinators      as Conduit
import qualified Data.Map.Merge.Strict         as Map
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Typeable                  ( Proxy(..) )

import           Reactive.Banana
import           Reactive.Banana.Frameworks

import           ReactiveDaemon

import           Servant.Client                 ( Client
                                                , ClientM
                                                , ClientEnv
                                                , client
                                                , hoistClient
                                                , runClientM
                                                )

runClient :: ClientEnv -> ZWave Moment (Event (IO ())) -> IO ()
runClient cenv cfg = do
    (eventHandler, write) <- newAddHandler
    let client  = clientIO cenv
        netDesc = dNetwork client cfg eventHandler
    compile netDesc >>= actuate
    void . handleState client $ do
        liftIO $ putStrLn "Connected to server."
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
