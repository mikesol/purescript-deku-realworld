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