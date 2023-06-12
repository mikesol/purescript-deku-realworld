module Main where

import Prelude

import API.Effects
  ( comments
  , getArticle
  , getArticles
  , getArticlesWithAuthor
  , getArticlesWithFavorited
  , getProfile
  , getTags
  )
import API.Types (AuthState(..), maybeToAuthState, mostRecentCurrentUser)
import Components.Article (ArticleStatus(..), article)
import Components.Create (create)
import Components.Footer (footer)
import Components.Home (ArticleLoadStatus(..), TagsLoadStatus(..), home)
import Components.Login (login)
import Components.Nav (nav)
import Components.Profile (ProfileStatus(..), profile)
import Components.Register (register)
import Components.Settings (settings)
import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))
import Data.Tuple (curry, snd)
import Data.Tuple.Nested ((/\))
import Deku.Control ((<#~>), (<$~>))
import Deku.Core (fixed)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.AffToEvent (affToEvent)
import FRP.Event (burning)
import FRP.Event as Event
import Route (route, Route(..))
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)
import Simple.JSON as JSON
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)

-- data Page = Home | Login | Profile | Settings | Footer | Create | Nav | Article

main :: Effect Unit
main = do
  storedUser <- ((_ >>= JSON.readJSON_) >>> maybeToAuthState) <$> (window >>= localStorage >>= getItem "session")
  routeEvent <- Event.create >>= \{ event, push } ->
    matchesWith (parse route) (curry push)
      *> map _.event (burning (Nothing /\ Home) event)
  currentUser <- Event.create >>= \{ event, push } -> do
    burning storedUser event <#> _.event >>> { push, event: _ }
  let
    logOut = do
      window >>= localStorage >>= removeItem "session"
      currentUser.push SignedOut
    setUser cu = do
      window >>= localStorage >>= setItem "session" (JSON.writeJSON cu)
      currentUser.push (SignedIn cu)
    mostRecentUser = mostRecentCurrentUser currentUser.event
  runInBody
    ( fixed
        [ nav logOut (map snd routeEvent) currentUser.event
        , D.div_
            [ ( routeEvent <#~> case _ of
                  _ /\ Home -> home currentUser.event
                    (pure ArticlesLoading <|> (ArticlesLoaded <$> affToEvent getArticles))
                    (pure TagsLoading <|> TagsLoaded <$> affToEvent getTags)
                  _ /\ Article slug -> D.div_
                    [ (article currentUser.event) <$~>
                        ( pure ArticleLoading <|>
                            ( ArticleLoaded
                                <$> affToEvent (getArticle slug)
                                <*> affToEvent (_.comments <$> comments slug)
                            )
                        )
                    ]
                  _ /\ Settings -> settings mostRecentUser setUser
                  _ /\ Editor -> create mostRecentUser
                  _ /\ LogIn -> login setUser
                  _ /\ Register -> register setUser
                  _ /\ Profile username -> D.div_
                    [ (profile currentUser.event) <$~>
                        ( pure ProfileLoading <|>
                            ( ProfileLoaded
                                <$> affToEvent (getProfile username)
                                <*> affToEvent (getArticlesWithAuthor username)
                                <*> affToEvent (getArticlesWithFavorited username)
                            )
                        )
                    ]
              )
            ]
        , footer
        ]
    )