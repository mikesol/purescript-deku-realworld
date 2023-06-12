# Functional programming

Deku is a micro-framework. It relies heavily on existing PureScript libraries such as `hyrule` and `effect`, both of which are over seven years old and exploit battle-tested functional patterns.

We've already gone over how Deku uses [`Event`-s](./events.md) from [`hyrule`](https://github.com/mikesol/purescript-hyrule). This document will explore some other common libraries used in Deku apps.

## Effect

Deku callbacks, like `OnClick` and `OnFocus`, are executed in the `Effect` monad. `Effect` is similar to `IO` in Haskell and represents a computation where each computation has a synchronous side-effect, like printing to the console or retrieving information from a DOM element, that exists outside of a pure functional context.

In the following example, the `Effect` monad is used to echo the contents of a `textarea` to a `div` whenever a `button` is pressed. This is done using the `value` function.

[Try Me](https://try.purescript.org/?github=/mikesol/purescript-deku-realworld/main/gh-examples/FP1.purs)
```purescript
module FP1 where

import Prelude

import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do (useState')
import Deku.Do as Deku
import Deku.Listeners (click, injectElementT)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Web.HTML.HTMLTextAreaElement (value)

app :: Nut
app = Deku.do
  setTextAreaElt /\ textAreaElt <- useState'
  setDivText /\ divText <- useState'
  D.div_
    [ D.textarea (injectElementT setTextAreaElt) []
    , D.button (click $ textAreaElt <#> (value >=> setDivText))
        [ text_ "Echo text area text"
        ]
    , D.div_ [ text divText ]
    ]

main :: Effect Unit
main = runInBody app
```

## Applicative validators

Forms are ubiqutous in web programming, and Deku apps rely on standard PureScript validation libraries to validate and parse data. In this RealWorld app, the following libraries are used:

- [`purescript-validation`](https://github.com/purescript/purescript-validation)
- [`purescript-simple-json`](https://github.com/justinwoo/purescript-simple-json)

In the example below, the `validation` library is used to validate a form on submit.

[Try Me](https://try.purescript.org/?github=/mikesol/purescript-deku-realworld/main/gh-examples/FP2.purs)
```purescript
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
```

## Generic codecs

PureScript has a mature routing ecosystem that most Deku apps use for their routing. This RealWorld app uses [`routing-duplex`](https://github.com/natefaubion/purescript-routing-duplex), which makes use of generic codecs to simplify routing.

Because routing deals with top-level URLs, it is difficult to make a stand-alone example without accidentally routing you off of the example, so to see how this works, you can check out the [router](../src/Route.purs) and the [main function](../src/Main.purs) in the RealWorld app.