module FRP.AffToEvent where

import Prelude

import Effect.Aff (Aff, error, killFiber, launchAff, launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (Event, makeEvent)

affToEvent :: Aff ~> Event
affToEvent a = makeEvent \k -> do
  fib <- launchAff (a >>= liftEffect <<< k)
  pure (launchAff_ (killFiber (error "Event unsubscribed") fib))