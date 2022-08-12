module API.Types where

import Data.Maybe (Maybe)

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

type SingleArticle = { article :: Article }

type MultipleArticles = { articles :: Array Article }