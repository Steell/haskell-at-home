{-# LANGUAGE TypeOperators #-}

module Client2 (
    runClient
) where

import           Api                            ( ConduitClient
                                                , API
                                                , DeviceId
                                                , HomeId
                                                , HomeMap
                                                , Value
                                                , ValueId
                                                , ValueState
                                                )

import ClientApi

import Control.Concurrent.Async (async)
import Control.Monad (void)
import Control.Monad.Reader (MonadReader, ReaderT, asks)

import qualified Data.Conduit.Combinators as Conduit
import           Data.Typeable                  ( Proxy(..) )

import Reactive.Banana
import Reactive.Banana.Frameworks

import ReactiveDaemon

import           Servant.API                    ( (:<|>)(..) )
import           Servant.Client                 ( Client, ClientM, ClientEnv, client, hoistClient, runClientM )

clientIO :: ClientEnv -> Client IO API
clientIO cenv = hoistClient api runClientIO (client api)
  where
    runClientIO :: ClientM a -> IO a
    runClientIO = fmap (either (error . show) id)
                . flip runClientM cenv         
    api :: Proxy API
    api = Proxy

runClient :: ClientEnv -> ZWave Moment (Event (IO ())) -> IO ()
runClient cenv cfg = do
    (eventHandler, write) <- newAddHandler
    let client = clientIO cenv
        netDesc = dNetwork client cfg eventHandler
    void . async . handleState client $ Conduit.mapM_ (liftIO . write)
    compile netDesc >>= actuate