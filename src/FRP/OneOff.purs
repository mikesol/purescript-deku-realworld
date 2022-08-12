module FRP.OneOff where

import Prelude

import Control.Monad.ST.Class (class MonadST)
import Data.Compactable (compact)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import FRP.EmitUntil (emitUntil)
import FRP.Event (AnEvent, mapAccum)

oneOff
  :: forall s m a b
   . MonadST s m
  => (a -> Maybe b)
  -> AnEvent m a
  -> AnEvent m b
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
