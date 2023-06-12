module Components.Following where

import Prelude

import API.Effects (follow, unfollow)
import API.Types (AuthState, isSignedIn, whenSignedIn)
import Data.Foldable (oneOf)
import Deku.Attribute (Attribute, (:=))
import Deku.Control (text)
import Deku.Core (Nut, fixed)
import Deku.Attributes (klass_)
import Deku.DOM as D
import Deku.Listeners (click)
import Effect (Effect)
import Effect.Aff (launchAff_)
import FRP.Event (Event)

followAttrs
  :: String
  -> Event AuthState
  -> Event Boolean
  -> (Boolean -> Effect Unit)
  -> Event (Attribute D.Button_)
followAttrs username currentUser isFollowing setFollowing = oneOf
  [ klass_  "btn btn-sm btn-outline-secondary"
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

followText ::  Event Boolean -> Nut
followText isFollowing = fixed [text (isFollowing <#> if _ then "Following" else "Follow")]
