module Events3 where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf)
import Deku.Attribute ((:=))
import Deku.Control (text)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Pursx (nut, (~~))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Event (fold, fromEvent)
import FRP.Event.Time (interval)
import Type.Proxy (Proxy(..))

template =
  Proxy
    :: Proxy
         """
<div>
  <h1 ~h1Atts~>Hello world!</h1>
  <p><i>This is</i>~txt~</p>
</div>
"""

app :: Nut
app = template ~~
  { h1Atts: oneOf
      [ pure $ D.Style := "style=\"color:blue;\""
      , pure $ D.Id := "my-h1"
      ]
  , txt: nut
      ( D.span_
          [ text do
              let counter n = fold (const (add 1)) (interval n) 0 <|> pure 0
              ( fromEvent
                  $
                    ( { a: _, b: _, c: _, d: _, e: _ }
                        <$> counter 100
                        <*> counter 200
                        <*> counter 400
                        <*> counter 800
                        <*> counter 1600
                    ) <#> (("Counting up: " <> _) <<< show)
              )
          ]
      )
  }

main :: Effect Unit
main = runInBody app