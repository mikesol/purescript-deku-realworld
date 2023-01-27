module API.Effects where

import Prelude

import API.Types (Comment, CreateArticle, MultipleArticles, MultipleComments, Profile, RegistrationRequest, RegistrationResponse, SignInRequest, SignInResponse, SingleProfile, UpdateUserRequest, SingleArticle)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, error, throwError)
import Fetch as Fetch
import Fetch.Yoga.Json as FetchYoga
import Foreign (Foreign)
import Foreign.Object (Object)
import Prim.Row (class Nub, class Union)
import Prim.RowList (class RowToList)
import Record (merge)
import Type.Row.Homogeneous (class Homogeneous, class HomogeneousRowList)
import Yoga.JSON as JSON

simpleGetOrDelete'
  :: forall r headers
   . JSON.ReadForeign r
  => Homogeneous headers String
  => Method
  -> { | headers }
  -> String
  -> Aff r
simpleGetOrDelete' method headers url = do
  { json } <- Fetch.fetch url
    { method
    , headers
    }
  response :: r <- FetchYoga.fromJSON json
  pure response

simpleGet'
  :: forall r headers
   . JSON.ReadForeign r
  => Homogeneous headers String
  => { | headers }
  -> String
  -> Aff r
simpleGet' = simpleGetOrDelete' GET

simpleDelete'
  :: forall r headers headersRL
   . JSON.ReadForeign r
  => RowToList headers headersRL
  => Homogeneous headers String
  => HomogeneousRowList headersRL String
  => { | headers }
  -> String
  -> Aff r
simpleDelete' = simpleGetOrDelete' DELETE

simpleGet :: forall r. JSON.ReadForeign r => String -> Aff r
simpleGet = simpleGet' {}

simplePostOrPut'
  :: forall i o headers newHeaders
   . JSON.WriteForeign i
  => JSON.ReadForeign o
  => Homogeneous newHeaders String
  => Union headers ("Content-Type" :: String) newHeaders
  => Nub newHeaders newHeaders
  => Method
  -> { | headers }
  -> String
  -> Maybe i
  -> Aff (PostReturn o)
simplePostOrPut' method headers url payload = do
  let
    newHeaders :: { | newHeaders }
    newHeaders = merge headers { "Content-Type": "application/json" }
  { text } <- Fetch.fetch url
    { body: JSON.writeJSON payload
    , method
    , headers: newHeaders
    }
  body <- text
  case JSON.readJSON body of
    Right (r :: o) -> pure (Right r)
    Left e -> case JSON.readJSON body of
      Right (r :: Errors) -> pure (Left r)
      Left e -> throwError $ error $ "Can't parse JSON. " <> show e

simplePost' :: forall i o headers newHeaders newHeadersRL
   . JSON.WriteForeign i
  => JSON.ReadForeign o
  => Union headers ("Content-Type" :: String) newHeaders
  => Nub newHeaders newHeaders
  => RowToList newHeaders newHeadersRL
  => HomogeneousRowList newHeadersRL String
  => { | headers } -> String -> i -> Aff (PostReturn o)
simplePost' h u = simplePostOrPut' POST h u <<< Just

simplePostNoBody' :: forall o headers newHeaders newHeadersRL
   . JSON.ReadForeign o
  => Union headers ("Content-Type" :: String) newHeaders
  => Nub newHeaders newHeaders
  => RowToList newHeaders newHeadersRL
  => HomogeneousRowList newHeadersRL String
  => { | headers } -> String -> Aff (PostReturn o)
simplePostNoBody' h u = simplePostOrPut' POST h u (Nothing :: Maybe {})

simplePut' :: forall i o headers newHeaders newHeadersRL. JSON.WriteForeign i
  => JSON.ReadForeign o
  => Union headers ("Content-Type" :: String) newHeaders
  => Nub newHeaders newHeaders
  => RowToList newHeaders newHeadersRL
  => HomogeneousRowList newHeadersRL String
  => { | headers } -> String -> i -> Aff (PostReturn o)
simplePut' h u = simplePostOrPut' PUT h u <<< Just

simplePost :: forall i o. JSON.WriteForeign i => JSON.ReadForeign o => String -> i -> Aff (PostReturn o)
simplePost u = simplePost' {} u <<< Just

type Errors = { errors :: Object (Array String) }
type PostReturn a = Either Errors a

getArticles :: Aff MultipleArticles
getArticles = simpleGet "https://api.realworld.io/api/articles"

getArticleFeed :: String -> Aff MultipleArticles
getArticleFeed token = simpleGet' { "Authorization": "Token " <> token } "https://api.realworld.io/api/articles/feed"

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
updateUser token payload = simplePut' { "Authorization": "Token " <> token } "https://api.realworld.io/api/user" payload

follow :: String -> String -> Aff (PostReturn { profile :: Profile })
follow token user = simplePostNoBody' { "Authorization": "Token " <> token } ("https://api.realworld.io/api/profiles/" <> user <> "/follow")

unfollow :: String -> String -> Aff { profile :: Profile }
unfollow token user = simpleDelete' { "Authorization": "Token " <> token } ("https://api.realworld.io/api/profiles/" <> user <> "/follow")

favorite :: String -> String -> Aff (PostReturn SingleArticle)
favorite token slug = simplePostNoBody' { "Authorization": "Token " <> token } ("https://api.realworld.io/api/articles/" <> slug <> "/favorite")

unfavorite :: String -> String -> Aff SingleArticle
unfavorite token slug = simpleDelete' { "Authorization": "Token " <> token } ("https://api.realworld.io/api/articles/" <> slug <> "/favorite")

comments :: String -> Aff MultipleComments
comments slug = simpleGet ("https://api.realworld.io/api/articles/" <> slug <> "/comments")

addComment :: String -> String -> String -> Aff (PostReturn { comment :: Comment })
addComment token slug body = simplePost' { "Authorization": "Token " <> token } ("https://api.realworld.io/api/articles/" <> slug <> "/comments") { comment: { body } }

deleteComment :: String -> String -> Int -> Aff Unit
deleteComment token slug id = do
  _ :: Foreign <- simpleDelete' { "Authorization": "Token " <> token } ("https://api.realworld.io/api/articles/" <> slug <> "/comments/" <> show id)
  pure unit

getProfile :: String -> Aff SingleProfile
getProfile username = simpleGet ("https://api.realworld.io/api/profiles/" <> username)

createArticle :: String -> CreateArticle -> Aff (PostReturn SingleArticle)
createArticle token = simplePost' { "Authorization": "Token " <> token } ("https://api.realworld.io/api/articles")
