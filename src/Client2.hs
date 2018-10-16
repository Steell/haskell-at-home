{-# LANGUAGE TypeOperators #-}

module Client2 (
    handleState,
    getValue,
    setValue,
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

import           Data.Typeable                  ( Proxy(..) )

import           Servant.API                    ( (:<|>)(..) )
import           Servant.Client                 ( ClientM )
import           Servant.Client.Core            ( clientIn )

handleState :: ConduitClient HomeMap () -> ClientM ()
valueApi :: HomeId -> DeviceId -> ValueId -> ((ValueState -> ClientM Value) :<|> ClientM Value)
(handleState :<|> valueApi) = (Proxy :: Proxy API) `clientIn` Proxy

setValue :: HomeId -> DeviceId -> ValueId -> ValueState -> ClientM Value
setValue h d v = fst $ valueApi h d v
  where fst (f :<|> _) = f

getValue :: HomeId -> DeviceId -> ValueId -> ClientM Value
getValue h d v = snd $ valueApi h d v
  where snd (_ :<|> s) = s
