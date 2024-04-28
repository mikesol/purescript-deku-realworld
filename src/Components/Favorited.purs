module Components.Favorited where

import Prelude

import API.Effects (favorite, unfavorite)
import API.Types (AuthState, whenSignedIn)
import Data.Foldable (for_)
import Deku.DOM.Listeners (runOn)
import Deku.DOM.Listeners as DL
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Poll (Poll)

doFavoriting
  :: Poll AuthState
  -> String
  -> Poll Boolean
  -> Poll Int
  -> (Int -> Effect Unit)
  -> (Boolean -> Effect Unit)
  -> Poll _
doFavoriting currentUser slug isFavorited favoritesCount setFavoritesCount setFavorited = runOn DL.click $
  ({ cu: _, fv: _, fc: _ } <$> currentUser <*> isFavorited <*> favoritesCount) <#> \{ cu, fv, fc } -> do
    whenSignedIn cu \cu' -> do
      setFavoritesCount (fc + if fv then -1 else 1)
      setFavorited (not fv)
      launchAff_ do
        if fv then do
          r <- unfavorite cu'.token slug
          liftEffect $ setFavoritesCount r.article.favoritesCount
        else do
          r <- favorite cu'.token slug
          liftEffect $ for_ r (_.article.favoritesCount >>> setFavoritesCount)
