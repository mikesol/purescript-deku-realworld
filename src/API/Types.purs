module API.Types where

import Prelude

import Data.Filterable (filterMap)
import Data.Maybe (Maybe(..))
import FRP.Poll (Poll)

type User =
  { email :: String
  , token :: String
  , username :: String
  , bio :: Maybe String
  , image :: Maybe String
  }

type Article =
  { slug :: String
  , title :: String
  , description :: String
  , body :: String
  , tagList :: Array String
  , createdAt :: String
  , updatedAt :: String
  , favorited :: Boolean
  , favoritesCount :: Int
  , author ::
      { username :: String
      , bio :: Maybe String
      , image :: String
      , following :: Boolean
      }
  }

type Profile =
  { username :: String
  , bio :: Maybe String
  , image :: String
  , following :: Boolean
  }

type Comment =
  { id :: Int
  , createdAt :: String
  , updatedAt :: String
  , body :: String
  , author :: Profile
  }

type SingleComment = { comment :: Comment }

type MultipleComments = { comments :: Array Comment }

type SingleProfile = { profile :: Profile }

-- endpoint

type SignInRequest = { user :: { email :: String, password :: String } }

type SignInResponse = { user :: User }

type RegistrationRequest = { user :: { email :: String, password :: String, username :: String } }

type RegistrationResponse = { user :: User }

type UpdateUserRequest =
  { user ::
      { email :: Maybe String
      , password :: Maybe String
      , username :: Maybe String
      , bio :: Maybe String
      , image :: Maybe String
      }
  }

type UpdateUserResponse = { user :: User }

type SingleArticle = { article :: Article }

type MultipleArticles = { articles :: Array Article }

type CreateArticle =
  { article ::
      { title :: String
      , description :: String
      , body :: String
      , tagList :: Array String
      }
  }

data AuthState = SignedIn User | SignedOut

isSignedIn :: AuthState -> Boolean
isSignedIn (SignedIn _) = true
isSignedIn SignedOut = false

whenSignedIn
  :: forall m
   . Applicative m
  => AuthState
  -> (User -> m Unit)
  -> m Unit
whenSignedIn u f = case u of
  SignedIn user -> f user
  SignedOut -> pure unit

isSignedOut :: AuthState -> Boolean
isSignedOut = not isSignedIn

maybeToAuthState :: Maybe User -> AuthState
maybeToAuthState Nothing = SignedOut
maybeToAuthState (Just user) = SignedIn user

mostRecentCurrentUser :: Poll AuthState -> Poll User
mostRecentCurrentUser = filterMap case _ of
  SignedIn user -> Just user
  SignedOut -> Nothing