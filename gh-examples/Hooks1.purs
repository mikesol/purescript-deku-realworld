module Hooks1 where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf)
import Data.Tuple.Nested ((/\))
import Deku.Attribute ((:=))
import Deku.Control (text)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do (useState, useState')
import Deku.Do as Deku
import Deku.Listeners (click)
import Deku.Pursx (nut, (~~))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Event (fold)
import Type.Proxy (Proxy(..))

template =
  Proxy :: Proxy """
<div>
  <h1 ~h1Atts~>Hello world!</h1>
  <p>Counter: ~counterTxt~</p>
  <button ~button1~>Change color</button>
  <button ~button2~>~button2Txt~</button>
</div>
"""

app :: Nut
app = Deku.do
  setCounter /\ counter <- useState'
  setColor /\ color <- useState "blue"
  template ~~
    { h1Atts: oneOf
        [ color <#> \c -> D.Style := "color:" <> c <> ";"
        , id_ := "my-h1"
        ]
    , counterTxt: nut (D.span_ [ text (fold (const (add 1)) counter (-1) <#> show) ])
    , button1: oneOf
        [ click $ color <#> \c -> do
            setColor (if c == "blue" then "red" else "blue")
        ]
    , button2: oneOf [ click $ pure $ setCounter unit ]
    , button2Txt: nut
        ( D.span_
            [ text
                ( ((counter $> false) <|> pure true)
                    <#> if _ then "Start counter" else "Increment counter"
                )
            ]
        )
    }

main :: Effect Unit
main = runInBody app
