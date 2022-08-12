module Route where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Routing.Duplex (RouteDuplex', root, segment, string)
import Routing.Duplex.Generic as G
import Routing.Duplex.Generic.Syntax ((/))

data Route = Home | Article String

derive instance genericRoute :: Generic Route _
instance Show Route where
  show = genericShow

route :: RouteDuplex' Route
route = root $ G.sum
  { "Home": G.noArgs
  , "Article": "article" / (string segment)
  }