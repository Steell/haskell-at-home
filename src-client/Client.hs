{-# LANGUAGE TypeOperators #-}

module Client
    ( runClient
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

import           Reactive.Banana
import           Reactive.Banana.Frameworks

import           ReactiveDaemon

import           Servant.Client                 ( ClientEnv )

runClient :: ClientEnv -> ZWave MomentIO () -> IO ()
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
