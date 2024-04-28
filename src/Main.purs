module Main where

import Prelude

import API.Effects (comments, getArticle, getArticles, getArticlesWithAuthor, getArticlesWithFavorited, getProfile, getTags)
import API.Types (AuthState(..), maybeToAuthState, mostRecentCurrentUser)
import Components.Article (ArticleStatus(..), makeArticle)
import Components.Create (create)
import Components.Footer (footer)
import Components.Home (ArticleLoadStatus(..), TagsLoadStatus(..), home)
import Components.Login (login)
import Components.Nav (nav)
import Components.Profile (ProfileStatus(..), makeProfile)
import Components.Register (register)
import Components.Settings (settings)
import Control.Alt ((<|>))
import Control.Monad.ST.Class (liftST)
import Control.Parallel (parSequence_, parallel, sequential)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Deku.Core (fixed)
import Deku.DOM as D
import Deku.Hooks (cycle)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Aff (Aff, killFiber, launchAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref as Ref
import FRP.Poll as Poll
import Route (route, Route(..))
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)
import Yoga.JSON as JSON

-- data Page = Home | Login | Profile | Settings | Footer | Create | Nav | Article

run ∷ forall a r. Aff a → { push ∷ a -> Effect Unit | r } → Aff Unit
run aff { push } = aff >>= liftEffect <<< push

main :: Effect Unit
main = do
  currentRoute <- liftST Poll.create
  currentUser <- liftST Poll.create
  let
    setUser cu = do
      window >>= localStorage >>= setItem "session" (JSON.writeJSON cu)
      currentUser.push (SignedIn cu)
  let mostRecentUser = mostRecentCurrentUser currentUser.poll
  articles <- liftST Poll.create
  tags <- liftST Poll.create
  article <- liftST Poll.create
  profile <- liftST Poll.create
  prevAction <- Ref.new (pure unit)
  let
    matcher _ r = do
      storedUser <- ((_ >>= JSON.readJSON_) >>> maybeToAuthState) <$> (window >>= localStorage >>= getItem "session")
      let currentUserPoll = pure storedUser <|> currentUser.poll
      pa <- Ref.read prevAction
      newAction <- launchAff $ killFiber (error "no fiber for you") pa *> parSequence_ case r of
        Home -> [ run (ArticlesLoaded <$> getArticles) articles, run (TagsLoaded <$> getTags) tags ]
        Article slug -> [ run (sequential (ArticleLoaded <$> parallel (getArticle slug) <*> parallel (_.comments <$> comments slug))) article ]
        Profile username ->
          [ run
              ( sequential
                  ( ProfileLoaded
                      <$> (parallel $ getProfile username)
                      <*> (parallel $ getArticlesWithAuthor username)
                      <*> (parallel $ getArticlesWithFavorited username)
                  )
              )
              profile
          ]
        _ -> []
      Ref.write newAction prevAction
      currentRoute.push $ Tuple r case r of
        Home -> do
          home currentUserPoll (pure ArticlesLoading <|> articles.poll) (pure TagsLoading <|> tags.poll)
        Article _ -> D.div_ [ makeArticle currentUserPoll (pure ArticleLoading <|> article.poll) ]
        Settings -> settings mostRecentUser setUser
        Editor -> create mostRecentUser
        LogIn -> login setUser
        Register -> register setUser
        Profile _ -> D.div_ [ makeProfile currentUserPoll (pure ProfileLoading <|> profile.poll) ]
  _ <- matchesWith (parse route) matcher
  let
    logOut = do
      window >>= localStorage >>= removeItem "session"
      currentUser.push SignedOut
  storedUser <- ((_ >>= JSON.readJSON_) >>> maybeToAuthState) <$> (window >>= localStorage >>= getItem "session")
  let currentUserPoll = pure storedUser <|> currentUser.poll
  runInBody
    ( fixed
        [ nav logOut (fst <$> currentRoute.poll) currentUserPoll
        , D.div_ [ cycle (snd <$> currentRoute.poll) ]
        , footer
        ]
    )
  matcher Nothing Home