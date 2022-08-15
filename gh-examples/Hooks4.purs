module Hooks4 where

import Prelude

import Data.Tuple.Nested ((/\))
import Deku.Attribute ((:=))
import Deku.Control (switcher_, text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do (useState)
import Deku.Do as Deku
import Deku.Listeners (click)
import Deku.Toplevel (runInBody)
import Effect (Effect)

data Page = A | B | C | D

pageToString :: Page -> String
pageToString A = "A"
pageToString B = "B"
pageToString C = "C"
pageToString D = "D"

app :: Nut
app = Deku.do
  setPage /\ page <- useState A
  D.div_
    [ D.div_
        ( map
            ( \v -> D.button (click $ pure (setPage v))
                [ text_ (pageToString v) ]
            )
            [ A, B, C, D ]
        )
    , page # switcher_ D.div case _ of
        A -> text_ "Page A"
        B -> D.a (pure $ D.Href := "https://github.com/mikesol/purescript-deku") [ text_ "Page B navigates to the Deku Github Repo." ]
        C -> D.button_ [ text_ "Page C is a button that does nothing" ]
        D -> D.div_ [ D.h1_ [ text_ "Inception!", D.div_ [ app ] ] ]
    ]

main :: Effect Unit
main = runInBody app