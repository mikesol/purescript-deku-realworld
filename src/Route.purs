module Route where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Routing.Duplex (RouteDuplex', path, root, segment, string)
import Routing.Duplex.Generic as G
import Routing.Duplex.Generic.Syntax ((/))

data Route = Home | Settings | Editor | Article String | LogIn | Register | Profile String

derive instance Eq Route
derive instance Ord Route

derive instance genericRoute :: Generic Route _
instance Show Route where
  show = genericShow

route :: RouteDuplex' Route
route = root $ G.sum
  { "Home": G.noArgs
  , "Article": "article" / (string segment)
  , "Profile": "profile" / (string segment)
  , "Editor": path "editor" G.noArgs
  , "Settings": path "settings" G.noArgs
  , "LogIn": path "login" G.noArgs
  , "Register": path "register" G.noArgs
  }