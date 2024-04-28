module Components.Nav where

import Prelude

import API.Types (AuthState, isSignedIn, isSignedOut)
import Deku.Control (text_)
import FRP.Poll (Poll)
import Deku.DOM.Attributes as DA
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.DOM.Listeners as DL
import Deku.DOM.Combinators (runOn_)
import Effect (Effect)
import FRP.Dedup (dedup)
import Route (Route(..))

nav
  :: Effect Unit
  -> Poll Route
  -> Poll AuthState
  -> Nut
nav logOut route currentUser = D.nav [ DA.klass_ "navbar navbar-light" ]
  [ D.div [ DA.klass_ "container" ]
      [ D.a [ DA.klass_ "navbar-brand", DA.href_ "/#/" ] [ text_ "conduit" ]
      , D.ul [ DA.klass_ "nav navbar-nav pull-xs-right" ]
          [ navItem Home "/#/" "Home" "nav-home" (pure true)
          , navItem Editor "/#/editor" "Editor" "nav-editor" (isSignedIn <$> currentUser)
          , navItem Settings "/#/settings" "Settings" "nav-settings" (isSignedIn <$> currentUser)
          , navItem LogIn "/#/login" "Sign in" "nav-sign-in" (isSignedOut <$> currentUser)
          , navItem Register "/#/register" "Sign up" "nav-sign-up" (isSignedOut <$> currentUser)
          , D.li
              [ DA.klass_ "nav-item"
              , doDisplay (isSignedIn <$> currentUser)
              ]
              [ D.a
                  [ DA.href_ "/#/"
                  , DA.klass_ "nav-link"
                  , runOn_ DL.click logOut
                  ]
                  [ text_ "Log out" ]
              ]
          ]
      ]
  ]
  where

  doDisplay :: _ Boolean -> _
  doDisplay displayCondition = DA.style $ dedup displayCondition <#> (if _ then "" else "display: none;")

  navItem :: Route -> String -> String -> String -> Poll Boolean -> Nut
  navItem myRoute href label id displayCondition = D.li
    [ DA.klass_ "nav-item"
    , DA.id_ id
    , doDisplay displayCondition
    ]
    [ D.a
        [ DA.href_ href
        , DA.klass $
            dedup
              ( map (eq myRoute >>> if _ then " active" else "")
                  (route)
              ) <#> \r -> "nav-link" <> r
        ]
        [ text_ label ]
    ]