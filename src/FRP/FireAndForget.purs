module FRP.FireAndForget where

import Prelude

import Data.Maybe (Maybe(..))
import FRP.Event (Event)
import FRP.OneOff (oneOff)

fireAndForget :: Event ~> Event
fireAndForget = oneOff Just
