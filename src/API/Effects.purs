module API.Effects where

import Prelude

import API.Types (MultipleArticles, SingleArticle)
import Affjax.ResponseFormat (string)
import Affjax.Web (get, printError)
import Data.Either (Either(..))
import Effect.Aff (Aff, error, throwError)
import Simple.JSON as JSON

simpleGet :: forall r. JSON.ReadForeign r => String -> Aff r
simpleGet url = do
  res <- get string url
  case res of
    Left err -> do
      throwError $ error $ "GET /api response failed to decode: " <> printError err
    Right response -> do
      case JSON.readJSON response.body of
        Right (r :: r) -> pure r
        Left e -> do
          throwError $ error $ "Can't parse JSON. " <> show e

getArticles :: Aff MultipleArticles
getArticles = simpleGet "https://api.realworld.io/api/articles"

getArticle :: String -> Aff SingleArticle
getArticle slug = simpleGet ("https://api.realworld.io/api/articles/" <> slug)