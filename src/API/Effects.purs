module API.Effects where

import Prelude

import API.Types (MultipleArticles, RegistrationRequest, RegistrationResponse, SignInRequest, SingleArticle, SignInResponse)
import Affjax.RequestBody as RequestFormat
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web (defaultRequest, get, printError, request)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationJSON)
import Effect.Aff (Aff, error, throwError)
import Simple.JSON as JSON

simpleGet :: forall r. JSON.ReadForeign r => String -> Aff r
simpleGet url = do
  res <- get ResponseFormat.string url
  case res of
    Left err -> do
      throwError $ error $ "GET /api response failed to decode: " <> printError err
    Right response -> do
      case JSON.readJSON response.body of
        Right (r :: r) -> pure r
        Left e -> do
          throwError $ error $ "Can't parse JSON. " <> show e

simplePost :: forall i o. JSON.WriteForeign i => JSON.ReadForeign o => String -> i -> Aff o
simplePost url payload = do
  res <- request
    ( defaultRequest
        { content = Just (RequestFormat.string (JSON.writeJSON payload))
        , responseFormat = ResponseFormat.string
        , method = Left POST
        , url = url
        , headers = [ ContentType applicationJSON ]
        }
    )
  case res of
    Left err -> do
      throwError $ error $ "GET /api response failed to decode: " <> printError err
    Right response -> do
      case JSON.readJSON response.body of
        Right (r :: o) -> pure r
        Left e -> do
          throwError $ error $ "Can't parse JSON. " <> show e

getArticles :: Aff MultipleArticles
getArticles = simpleGet "https://api.realworld.io/api/articles"

getArticle :: String -> Aff SingleArticle
getArticle slug = simpleGet ("https://api.realworld.io/api/articles/" <> slug)

register :: RegistrationRequest -> Aff RegistrationResponse
register payload = simplePost "https://api.realworld.io/api/users" payload

logIn :: SignInRequest -> Aff SignInResponse
logIn payload = simplePost "https://api.realworld.io/api/users/login" payload