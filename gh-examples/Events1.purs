module Events1 where

import Prelude

import Data.Foldable (oneOf)
import Deku.Attribute ((:=))
import Deku.Control (text)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Pursx (nut, (~~))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Event (fromEvent)
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
          [ text
              ( fromEvent
                  $ interval 100 <#> (("Current time: " <> _) <<< show)
              )
          ]
      )
  }

main :: Effect Unit
main = runInBody app