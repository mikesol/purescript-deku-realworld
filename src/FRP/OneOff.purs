module FRP.OneOff where

import Prelude

import Data.Compactable (compact)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import FRP.EmitUntil (emitUntil)
import FRP.Event (Event, mapAccum)

oneOff
  :: forall a b
   . (a -> Maybe b)
  -> Event a
  -> Event b
oneOff f e = compact $ emitUntil identity
  ( mapAccum
      ( \a b -> case f a, b of
          _, true -> true /\ Nothing
          Nothing, false -> false /\ Just Nothing
          Just x, false -> true /\ Just (Just x)
      )
      e
      false
  )
