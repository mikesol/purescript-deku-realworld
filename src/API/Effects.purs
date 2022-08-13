module API.Effects where

import Prelude

import API.Types (Comment, MultipleArticles, MultipleComments, Profile, RegistrationRequest, RegistrationResponse, SignInRequest, SignInResponse, SingleArticle, UpdateUserRequest, SingleProfile)
import Affjax.RequestBody as RequestFormat
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web (defaultRequest, printError, request)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationJSON)
import Effect.Aff (Aff, error, throwError)
import Foreign (Foreign)
import Foreign.Object (Object)
import Simple.JSON as JSON

simpleGetOrDelete' :: forall r. JSON.ReadForeign r => Method -> Array RequestHeader -> String -> Aff r
simpleGetOrDelete' method headers url = do
  res <- request
    ( defaultRequest
        { responseFormat = ResponseFormat.string
        , method = Left method
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

simpleGet' :: forall r. JSON.ReadForeign r => Array RequestHeader -> String -> Aff r
simpleGet' = simpleGetOrDelete' GET

simpleDelete' :: forall r. JSON.ReadForeign r => Array RequestHeader -> String -> Aff r
simpleDelete' = simpleGetOrDelete' DELETE

simpleGet :: forall r. JSON.ReadForeign r => String -> Aff r
simpleGet = simpleGet' []

simplePostOrPut' :: forall i o. JSON.WriteForeign i => JSON.ReadForeign o => Method -> Array RequestHeader -> String -> Maybe i -> Aff (PostReturn o)
simplePostOrPut' method headers url payload = do
  res <- request
    ( defaultRequest
        { content = map (RequestFormat.string <<< JSON.writeJSON) payload
        , responseFormat = ResponseFormat.string
        , method = Left method
        , url = url
        , headers = [ ContentType applicationJSON ] <> headers
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

simplePost' :: forall i o. JSON.WriteForeign i => JSON.ReadForeign o => Array RequestHeader -> String -> i -> Aff (PostReturn o)
simplePost' h u = simplePostOrPut' POST h u <<< Just

simplePostNoBody' :: forall o. JSON.ReadForeign o => Array RequestHeader -> String -> Aff (PostReturn o)
simplePostNoBody' h u = simplePostOrPut' POST h u (Nothing :: Maybe {})

simplePut' :: forall i o. JSON.WriteForeign i => JSON.ReadForeign o => Array RequestHeader -> String -> i -> Aff (PostReturn o)
simplePut' h u = simplePostOrPut' PUT h u <<< Just

simplePost :: forall i o. JSON.WriteForeign i => JSON.ReadForeign o => String -> i -> Aff (PostReturn o)
simplePost u = simplePost' [] u <<< Just

type Errors = { errors :: Object (Array String) }
type PostReturn a = Either Errors a

getArticles :: Aff MultipleArticles
getArticles = simpleGet "https://api.realworld.io/api/articles"

getArticleFeed :: String -> Aff MultipleArticles
getArticleFeed token = simpleGet' [ RequestHeader "Authorization" ("Token " <> token) ] "https://api.realworld.io/api/articles/feed"

getArticlesWithTag :: String -> Aff MultipleArticles
getArticlesWithTag tag = simpleGet $ "https://api.realworld.io/api/articles?tag=" <> tag

getArticlesWithAuthor :: String -> Aff MultipleArticles
getArticlesWithAuthor tag = simpleGet $ "https://api.realworld.io/api/articles?author=" <> tag

getArticlesWithFavorited :: String -> Aff MultipleArticles
getArticlesWithFavorited tag = simpleGet $ "https://api.realworld.io/api/articles?favorited=" <> tag

getTags :: Aff { tags :: Array String }
getTags = simpleGet "https://api.realworld.io/api/tags"

getArticle :: String -> Aff SingleArticle
getArticle slug = simpleGet ("https://api.realworld.io/api/articles/" <> slug)

register :: RegistrationRequest -> Aff (PostReturn RegistrationResponse)
register payload = simplePost "https://api.realworld.io/api/users" payload

logIn :: SignInRequest -> Aff (PostReturn SignInResponse)
logIn payload = simplePost "https://api.realworld.io/api/users/login" payload

updateUser :: String -> UpdateUserRequest -> Aff (PostReturn SignInResponse)
updateUser token payload = simplePut' [ RequestHeader "Authorization" ("Token " <> token) ] "https://api.realworld.io/api/user" payload

follow :: String -> String -> Aff (PostReturn { profile :: Profile })
follow token user = simplePostNoBody' [ RequestHeader "Authorization" ("Token " <> token) ] ("https://api.realworld.io/api/profiles/" <> user <> "/follow")

unfollow :: String -> String -> Aff { profile :: Profile }
unfollow token user = simpleDelete' [ RequestHeader "Authorization" ("Token " <> token) ] ("https://api.realworld.io/api/profiles/" <> user <> "/follow")

favorite :: String -> String -> Aff (PostReturn SingleArticle)
favorite token slug = simplePostNoBody' [ RequestHeader "Authorization" ("Token " <> token) ] ("https://api.realworld.io/api/articles/" <> slug <> "/favorite")

unfavorite :: String -> String -> Aff SingleArticle
unfavorite token slug = simpleDelete' [ RequestHeader "Authorization" ("Token " <> token) ] ("https://api.realworld.io/api/articles/" <> slug <> "/favorite")

comments :: String -> Aff MultipleComments
comments slug = simpleGet ("https://api.realworld.io/api/articles/" <> slug <> "/comments")

addComment :: String -> String -> String -> Aff (PostReturn { comment :: Comment})
addComment token slug body = simplePost' [ RequestHeader "Authorization" ("Token " <> token) ] ("https://api.realworld.io/api/articles/" <> slug <> "/comments") { comment: { body } }

deleteComment :: String -> String -> Int -> Aff Unit
deleteComment token slug id = do
  _ :: Foreign <- simpleDelete' [ RequestHeader "Authorization" ("Token " <> token) ] ("https://api.realworld.io/api/articles/" <> slug <> "/comments/" <> show id)
  pure unit

getProfile :: String -> Aff SingleProfile
getProfile username = simpleGet ("https://api.realworld.io/api/profiles/" <> username)
