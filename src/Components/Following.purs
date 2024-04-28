module Components.Following where

import Prelude

import API.Effects (follow, unfollow)
import API.Types (AuthState, isSignedIn, whenSignedIn)
import Deku.Control (text)
import Deku.Core (Nut, fixed)
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners (runOn)
import Deku.DOM.Listeners as DL
import Effect (Effect)
import Effect.Aff (launchAff_)
import FRP.Poll (Poll)

followAttrs
  :: String
  -> Poll AuthState
  -> Poll Boolean
  -> (Boolean -> Effect Unit)
  -> Array (Poll _)
followAttrs username currentUser isFollowing setFollowing =
  [ DA.klass_  "btn btn-sm btn-outline-secondary"
  , DA.style $ currentUser <#> \cu -> if isSignedIn cu then "" else "display:none;"
  , runOn DL.click $ ({ cu: _, flw: _ } <$> currentUser <*> isFollowing) <#> \{ cu, flw } -> do
      whenSignedIn cu \cu' -> do
        setFollowing (not flw)
        launchAff_ do
          if flw then
            void $ unfollow cu'.token username
          else
            void $ follow cu'.token username
  ]

followText ::  Poll Boolean -> Nut
followText isFollowing = fixed [text (isFollowing <#> if _ then "Following" else "Follow")]
