module Main where

import Prelude

import API.Effects (getArticle, getArticles, getTags)
import Bolson.Control (switcher)
import Bolson.Core (envy)
import Components.Article (article)
import Components.Create (create)
import Components.Footer (footer)
import Components.Home (ArticleLoadStatus(..), TagsLoadStatus(..), home)
import Components.Login (login)
import Components.Nav (nav)
import Components.Profile (profile)
import Components.Register (register)
import Components.Settings (settings)
import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))
import Data.Tuple (curry, snd)
import Data.Tuple.Nested ((/\))
import Deku.DOM as D
import Deku.Toplevel (runInBodyA)
import Effect (Effect)
import FRP.AffToEvent (affToEvent)
import FRP.Event (burning, fromEvent, hot)
import FRP.Event as Event
import Route (route, Route(..))
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)

-- data Page = Home | Login | Profile | Settings | Footer | Create | Nav | Article

main :: Effect Unit
main = do
  routeEvent <- Event.create >>= \{ event, push } ->
    matchesWith (parse route) (curry push)
      *> map _.event (burning (Nothing /\ Home) event)
  currentUser <- Event.create >>= \{ event, push } -> do
    burning Nothing event <#> _.event >>> { push, event: _ }
  runInBodyA
    [ nav (currentUser.push Nothing) (map snd routeEvent) currentUser.event
    , D.div_
        [ ( routeEvent # switcher case _ of
              _ /\ Home -> home currentUser.event (ArticlesLoaded <$> affToEvent getArticles) (TagsLoaded <$> affToEvent getTags)
              _ /\ Article s -> envy $ fromEvent $ (map article (affToEvent (getArticle s)))
              _ /\ Settings -> settings
              _ /\ Editor -> create
              _ /\ LogIn -> login (Just >>> currentUser.push)
              _ /\ Register -> register (Just >>> currentUser.push)
              _ /\ Profile -> profile
          )
        ]
    , footer
    ]