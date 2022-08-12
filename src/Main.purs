module Main where

import Prelude

import API.Effects (getArticle, getArticles)
import API.Types (SingleArticle)
import Bolson.Control (switcher)
import Bolson.Core (envy)
import Components.Article (article)
import Components.Create (create)
import Components.Footer (footer)
import Components.Home (ArticleLoadStatus(..), home)
import Components.Login (login)
import Components.Nav (nav)
import Components.Profile (profile)
import Components.Settings (settings)
import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))
import Data.Tuple (curry)
import Data.Tuple.Nested ((/\))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.AffToEvent (affToEvent)
import FRP.Event (fromEvent)
import FRP.Event as Event
import Route (route, Route(..))
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)

-- data Page = Home | Login | Profile | Settings | Footer | Create | Nav | Article

main :: Effect Unit
main = do
  routeEvent <- Event.create
  _ <- matchesWith (parse route) (curry routeEvent.push)
  runInBody
    ( (routeEvent.event <|> pure (Nothing /\ Home)) # switcher case _ of
        _ /\ Home -> home (ArticlesLoaded <$> affToEvent (getArticles))
        _ /\ Article s -> envy $ fromEvent $ (map article (affToEvent (getArticle s)))
    )