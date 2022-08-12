module API.Effects where

import Prelude

import API.Types (MultipleArticles, RegistrationRequest, RegistrationResponse, SignInRequest, SingleArticle, SignInResponse)
import Affjax.RequestBody as RequestFormat
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web (defaultRequest, printError, request)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationJSON)
import Effect.Aff (Aff, error, throwError)
import Foreign.Object (Object)
import Simple.JSON as JSON

simpleGet' :: forall r. JSON.ReadForeign r => Array RequestHeader -> String -> Aff r
simpleGet' headers url = do
  res <- request
    ( defaultRequest
        { responseFormat = ResponseFormat.string
        , method = Left GET
        , url = url
        , headers = headers
        }
    )
  case res of
    Left err -> do
      throwError $ error $ "GET /api response failed to decode: " <> printError err
    Right response -> do
      case JSON.readJSON response.body of
        Right (r :: r) -> pure r
        Left e -> do
          throwError $ error $ "Can't parse JSON. " <> show e

simpleGet :: forall r. JSON.ReadForeign r => String -> Aff r
simpleGet = simpleGet' []

simplePost :: forall i o. JSON.WriteForeign i => JSON.ReadForeign o => String -> i -> Aff (PostReturn o)
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
    Left e -> throwError (error (printError e))
    Right r ->
      case JSON.readJSON r.body of
        Right (r :: o) -> pure (Right r)
        Left e -> case JSON.readJSON r.body of
          Right (r :: Errors) -> pure (Left r)
          Left e -> throwError $ error $ "Can't parse JSON. " <> show e

type Errors = { errors :: Object (Array String) }
type PostReturn a = Either Errors a

getArticles :: Aff MultipleArticles
getArticles = simpleGet "https://api.realworld.io/api/articles"

getArticleFeed :: String -> Aff MultipleArticles
getArticleFeed token = simpleGet' [ RequestHeader "Authorization" ("Token " <> token)] "https://api.realworld.io/api/articles/feed"

getArticlesWithTag :: String -> Aff MultipleArticles
getArticlesWithTag tag = simpleGet $ "https://api.realworld.io/api/articles?tag=" <> tag

getTags :: Aff { tags :: Array String }
getTags = simpleGet "https://api.realworld.io/api/tags"

getArticle :: String -> Aff SingleArticle
getArticle slug = simpleGet ("https://api.realworld.io/api/articles/" <> slug)

register :: RegistrationRequest -> Aff (PostReturn RegistrationResponse)
register payload = simplePost "https://api.realworld.io/api/users" payload

logIn :: SignInRequest -> Aff (PostReturn SignInResponse)
logIn payload = simplePost "https://api.realworld.io/api/users/login" payload