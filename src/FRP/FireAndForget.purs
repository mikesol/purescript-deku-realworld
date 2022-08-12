module FRP.FireAndForget where

import Prelude

import Control.Monad.ST.Class (class MonadST)
import FRP.OneOff (oneOff)
import Data.Maybe (Maybe(..))
import FRP.Event (AnEvent)

fireAndForget
  :: forall s m
   . MonadST s m
  => AnEvent m ~> AnEvent m
fireAndForget = oneOff Just
