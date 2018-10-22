module ClientApi where

import           Api

import           Servant.API
import           Servant.Client
import           Servant.Client.Core

handleState :: Monad m => Client m API -> ConduitClient HomeMap () -> m ()
handleState (f :<|> _) = f

setValue
    :: Monad m
    => Client m API
    -> HomeId
    -> DeviceId
    -> ValueId
    -> ValueState
    -> m ()
setValue (_ :<|> valueApi) h d v = fst $ valueApi h d v
    where fst (f :<|> _) = f

getValue :: Monad m => Client m API -> HomeId -> DeviceId -> ValueId -> m Value
getValue (_ :<|> valueApi) h d v = snd $ valueApi h d v
    where snd (_ :<|> s) = s
