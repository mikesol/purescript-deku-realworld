module FRP.EmitUntil where

import Prelude

import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Internal as Ref
import Data.Maybe (Maybe(..))
import FRP.Event (Event, makeLemmingEvent)

emitUntil
  :: forall a b
   . (a -> Maybe b)
  -> Event a
  -> Event b
emitUntil aToB e = makeLemmingEvent \sub k -> do
  r <- liftST $ Ref.new true
  u <- liftST $ Ref.new (pure unit)
  usu <- sub e \n -> do
    l <- liftST $ Ref.read r
    when l $ do
      case aToB n of
        Just b -> k b
        Nothing -> do
          void $ liftST $ Ref.write false r
          join (liftST $ Ref.read u)
          void $ liftST $ Ref.write (pure unit) u
  void $ liftST $ Ref.write usu u
  pure do
    join (liftST $ Ref.read u)
