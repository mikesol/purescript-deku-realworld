module Main where

import Prelude

import API.Effects (getArticles)
import Components.Article (article)
import Components.Create (create)
import Components.Footer (footer)
import Components.Home (ArticleLoadStatus(..), home)
import Components.Login (login)
import Components.Nav (nav)
import Components.Profile (profile)
import Components.Settings (settings)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.AffToEvent (affToEvent)

data Page = Home | Login | Profile | Settings | Footer | Create | Nav | Article
page = Home :: Page

main :: Effect Unit
main = do
  runInBody case page of
    Home -> home (ArticlesLoaded <$> affToEvent (getArticles))
    Login -> login
    Nav -> nav
    Profile -> profile
    Settings -> settings
    Footer -> footer
    Create -> create
    Article -> article