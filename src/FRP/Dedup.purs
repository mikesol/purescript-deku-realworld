module FRP.Dedup where

import Prelude

import Data.Compactable (class Compactable, compact)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import FRP.Event (class IsEvent, mapAccum)

dedup :: forall e a. Compactable e => IsEvent e => Eq a => e a -> e a
dedup e = compact $
  mapAccum (\a b -> let jb = Just b in Tuple a (if a == jb then Nothing else jb)) Nothing e