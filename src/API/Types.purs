module API.Types where

import Data.Maybe (Maybe)

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

-- endpoint

type SignInRequest = { user :: { email :: String, password :: String } }

type SignInResponse = { user :: User }

type RegistrationRequest = { user :: { email :: String, password :: String, username :: String } }

type RegistrationResponse = { user :: User }

type UpdateUserRequest =
  { email :: Maybe String
  , password :: Maybe String
  , username :: Maybe String
  , bio :: Maybe String
  , image :: Maybe String
  }

type UpdateUserResponse = { user :: User }

type SingleArticle = { article :: Article }

type MultipleArticles = { articles :: Array Article }