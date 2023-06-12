module Components.Nav where

import Prelude

import API.Types (AuthState, isSignedIn, isSignedOut)
import Deku.Attribute (Attribute, (:=), (!:=))
import Deku.Control (text_)
import Deku.Core (Nut, fixed)
import Deku.DOM as D
import Deku.Listeners (click_)
import Deku.Pursx ((~~))
import Effect (Effect)
import FRP.Dedup (dedup)
import FRP.Event (Event)
import Route (Route(..))
import Type.Proxy (Proxy(..))

nav_ =
  Proxy
    :: Proxy
         """<nav class="navbar navbar-light">
    <div class="container">
        <a class="navbar-brand" href="/#/">conduit</a>
        ~navbar~
    </div>
</nav>"""

nav
  :: Effect Unit
  -> Event Route
  -> Event AuthState
  -> Nut
nav logOut route currentUser = nav_ ~~
  { navbar: fixed
      [ D.ul [ D.Class !:= "nav navbar-nav pull-xs-right" ]
          [ navItem Home "/#/" "Home" "nav-home" (pure true)
          , navItem Editor "/#/editor" "Editor" "nav-editor" (isSignedIn <$> currentUser)
          , navItem Settings "/#/settings" "Settings" "nav-settings" (isSignedIn <$> currentUser)
          , navItem LogIn "/#/login" "Sign in" "nav-sign-in" (isSignedOut <$> currentUser)
          , navItem Register "/#/register" "Sign up" "nav-sign-up" (isSignedOut <$> currentUser)
          , D.li
              [ D.Class !:= "nav-item"
              , doDisplay (isSignedIn <$> currentUser)
              ]
              [ D.a
                  [ D.Href !:= "/#/"
                  , D.Class !:= "nav-link"
                  , click_ logOut
                  ]
                  [ text_ "Log out" ]
              ]
          ]
      ]
  }
  where

  doDisplay :: Event Boolean -> Event (Attribute D.Li_)
  doDisplay displayCondition = dedup displayCondition <#> ((if _ then "" else "display: none;") >>> (D.Style := _))

  navItem :: Route -> String -> String -> String -> Event Boolean -> Nut
  navItem myRoute href label id displayCondition = D.li
    [ D.Class !:= "nav-item"
    , D.Id !:= id
    , doDisplay displayCondition
    ]
    [ D.a
        [ D.Href !:= href
        , dedup
            ( map (eq myRoute >>> if _ then " active" else "")
                (route)
            ) <#> \r -> D.Class := "nav-link" <> r
        ]
        [ text_ label ]
    ]