module Main where

import Prelude

import API.Effects (comments, getArticle, getArticles, getArticlesWithAuthor, getArticlesWithFavorited, getProfile, getTags)
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
import Data.Tuple (Tuple(..), curry, snd)
import Data.Tuple.Nested ((/\))
import Deku.Core (fixed)
import Deku.DOM as D
import Deku.Hooks (cycle)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (killFiber, launchAff)
import Effect.Exception (error)
import Effect.Ref as Ref
import FRP.Event as Event
import FRP.Poll as Poll
import Route (route, Route(..))
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)
import Yoga.JSON as JSON

-- data Page = Home | Login | Profile | Settings | Footer | Create | Nav | Article

main :: Effect Unit
main = do
  routeEvent <- Poll.create
  currentUser <- Poll.create
  prevAction <- Ref.new (pure unit)
  matchesWith (parse route) \(_ /\ r) -> do
    storedUser <- ((_ >>= JSON.readJSON_) >>> maybeToAuthState) <$> (window >>= localStorage >>= getItem "session")
    pa <- Ref.read prevAction
    newAction <- launchAff do
      killFiber (error "no fiber for you") pa
      let currentUserPoll = pure storedUser <|> currentUser.event
      pure Tuple r <$> case r of
        Home -> home currentUserPoll
          (pure ArticlesLoading <|> (ArticlesLoaded <$> affToEvent getArticles))
          (pure TagsLoading <|> TagsLoaded <$> affToEvent getTags)
        Article slug -> D.div_
          [ article currentUserPoll
              ( pure ArticleLoading <|>
                  ( ArticleLoaded
                      <$> affToEvent (getArticle slug)
                      <*> affToEvent (_.comments <$> comments slug)
                  )
              )
          ]
        Settings -> settings mostRecentUser setUser
        Editor -> create mostRecentUser
        LogIn -> login setUser
        Register -> register setUser
        Profile username -> D.div_
          [ profile currentUserPoll
              ( pure ProfileLoading <|>
                  ( ProfileLoaded
                      <$> affToEvent (getProfile username)
                      <*> affToEvent (getArticlesWithAuthor username)
                      <*> affToEvent (getArticlesWithFavorited username)
                  )
              )
          ]
    Ref.write newAction prevAction
  let
    logOut = do
      window >>= localStorage >>= removeItem "session"
      currentUser.push SignedOut
    setUser cu = do
      window >>= localStorage >>= setItem "session" (JSON.writeJSON cu)
      currentUser.push (SignedIn cu)
    mostRecentUser = mostRecentCurrentUser currentUserPoll
  runInBody
    ( fixed
        [ nav logOut (fst <$> routeEvent) currentUserPoll
        , D.div_ [ cycle (snd <$> routeEvent) ]
        , footer
        ]
    )
  routeEvent.push Home