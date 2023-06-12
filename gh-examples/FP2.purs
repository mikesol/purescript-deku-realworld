module FP2 where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.String (Pattern(..), contains)
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (V, invalid, toEither)
import Deku.Attribute ((:=))
import Deku.Control (blank, switcher_, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do (useState')
import Deku.Do as Deku
import Deku.Listeners (click, injectElementT)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Web.HTML.HTMLInputElement (value)

data FormState = Errors (Array String) | Success | NoMessage

emailValidator  :: String -> V (Array String) String
emailValidator s = if contains (Pattern "@") s then pure s else invalid ["E-mail must have an @"]
urlValidator  :: String -> V (Array String) String
urlValidator s = if contains (Pattern "://") s then pure s else invalid ["Url mmust have ://"]

app :: Nut
app = Deku.do
  setEmailElt /\ emailElt <- useState'
  setUrlElt /\ urlElt <- useState'
  setMessage /\ message <- useState'
  let
    input placeholder cb = D.input
      ( oneOf
          [ D.Placeholder !:= placeholder
          , injectElementT cb
          , D.OnFocus !:= setMessage NoMessage
          ]
      )
      []
  D.div_
    [ input "E-mail" setEmailElt
    , input "URL" setUrlElt
    , message # switcher_ D.div case _ of
      NoMessage -> blank
      Errors e -> D.ul_ (map (\i -> D.li (style_ "color: red;") [text_ i]) e)
      Success -> D.p (style_ "color: green;") [text_ "Form submitted with no errors!"]
    , D.button
        ( oneOf
            [ click $ ({ email: _, url: _ } <$> emailElt <*> urlElt) <#> \fields -> do
                email <- value fields.email
                url <- value fields.url
                case toEither (emailValidator email *> urlValidator url) of
                  Left a -> setMessage (Errors a)
                  Right _ -> setMessage Success
            ]
        )
        [ text_ "Submit"
        ]
    ]

main :: Effect Unit
main = runInBody app