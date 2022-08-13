module Components.Nav where

import Prelude

import API.Types (AuthState, isSignedIn, isSignedOut)
import Data.Foldable (oneOf)
import Deku.Attribute (Attribute, (:=))
import Deku.Control (text_)
import Deku.Core (class Korok, Domable)
import Deku.DOM as D
import Deku.Listeners (click)
import Deku.Pursx (nut, (~~))
import Effect (Effect)
import FRP.Dedup (dedup)
import FRP.Event (AnEvent)
import Route (Route(..))
import Type.Proxy (Proxy(..))

nav_ =
  Proxy    :: Proxy
         """<nav class="navbar navbar-light">
    <div class="container">
        <a class="navbar-brand" href="/#/">conduit</a>
        ~navbar~
    </div>
</nav>"""

nav :: forall s m lock payload.
  Korok s m => Effect Unit -> AnEvent m Route -> AnEvent m AuthState -> Domable m lock payload
nav logOut route currentUser = nav_ ~~
  { navbar: nut
      ( D.ul (pure $ D.Class := "nav navbar-nav pull-xs-right")
          [ navItem Home "/#/" "Home" (pure true)
          , navItem Editor "/#/editor" "Editor" (isSignedIn <$> currentUser)
          , navItem Settings "/#/settings" "Settings" (isSignedIn <$> currentUser)
          , navItem LogIn "/#/login" "Sign in" (isSignedOut <$> currentUser)
          , navItem Register "/#/register" "Sign up" (isSignedOut <$> currentUser)
          , D.li
              ( oneOf
                  [ pure $ D.Class := "nav-item"
                  , doDisplay (isSignedIn <$> currentUser)
                  ]
              )
              [ D.a
                  ( oneOf
                      [ pure $ D.Href := "/#/"
                      , pure $ D.Class := "nav-link"
                      , click $ pure logOut
                      ]
                  )
                  [ text_ "Log out" ]
              ]
          ]
      )
  }
  where

  doDisplay :: AnEvent m Boolean -> AnEvent m (Attribute D.Li_)
  doDisplay displayCondition = dedup displayCondition <#> ((if _ then "" else "display: none;") >>> (D.Style := _))

  navItem :: Route -> String -> String -> AnEvent m Boolean -> Domable m lock payload
  navItem myRoute href label displayCondition = D.li
    ( oneOf
        [ pure $ D.Class := "nav-item"
        , doDisplay displayCondition
        ]
    )
    [ D.a
        ( oneOf
            [ pure $ D.Href := href
            , dedup
                ( map (eq myRoute >>> if _ then " active" else "")
                    (route)
                ) <#> \r -> D.Class := "nav-link" <> r
            ]
        )
        [ text_ label ]
    ]