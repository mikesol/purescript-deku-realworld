module FRP.Dedup where

import Prelude

import Data.Compactable (compact)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import FRP.Event (Event, mapAccum)

dedup :: forall a. Eq a => Event a -> Event a
dedup e = compact $
  mapAccum (\a b -> let jb = Just b in Tuple a (if a == jb then Nothing else jb)) Nothing e