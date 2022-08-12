module Main where

import Prelude

import Components.Create (create)
import Components.Footer (footer)
import Components.Home (home)
import Components.Login (login)
import Components.Nav (nav)
import Components.Profile (profile)
import Components.Settings (settings)
import Components.Article (article)
import Deku.Toplevel (runInBody)
import Effect (Effect)


data Page = Home | Login | Profile | Settings | Footer | Create | Nav | Article
page = Profile :: Page

main :: Effect Unit
main = runInBody case page of
  Home -> home
  Login -> login
  Nav -> nav
  Profile -> profile
  Settings -> settings
  Footer -> footer
  Create -> create
  Article -> article