module Components.Following where

import Prelude

import API.Effects (follow, unfollow)
import API.Types (AuthState, isSignedIn, whenSignedIn)
import Data.Foldable (oneOf)
import Deku.Attribute (Attribute, (:=))
import Deku.Control (text)
import Deku.Core (class Korok)
import Deku.DOM as D
import Deku.Listeners (click)
import Deku.Pursx (PursxElement, nut)
import Effect (Effect)
import Effect.Aff (launchAff_)
import FRP.Event (AnEvent)

followAttrs
  :: forall s m
   . Korok s m
  => String
  -> AnEvent m AuthState
  -> AnEvent m Boolean
  -> (Boolean -> Effect Unit)
  -> AnEvent m (Attribute D.Button_)
followAttrs username currentUser isFollowing setFollowing = oneOf
  [ pure $ D.Class := "btn btn-sm btn-outline-secondary"
  , currentUser <#> \cu -> D.Style := if isSignedIn cu then "" else "display:none;"
  , click $ ({ cu: _, flw: _ } <$> currentUser <*> isFollowing) <#> \{ cu, flw } -> do
      whenSignedIn cu \cu' -> do
        setFollowing (not flw)
        launchAff_ do
          if flw then
            void $ unfollow cu'.token username
          else
            void $ follow cu'.token username
  ]

followText :: forall s m lock payload. Korok s m => AnEvent m Boolean -> PursxElement m lock payload
followText isFollowing = nut (text (isFollowing <#> if _ then "Following" else "Follow"))
