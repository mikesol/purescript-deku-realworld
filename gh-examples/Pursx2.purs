module Pursx2 where

import Prelude

import Data.Foldable (oneOf)
import Deku.Attribute ((:=))
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Pursx (nut, (~~))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Type.Proxy (Proxy(..))

template =
  Proxy :: Proxy """
<div>
  <h1 ~h1Atts~>Hello world!</h1>
  <p><i>This is</i>~txt~</p>
</div>
"""

app :: Nut
app = template ~~
  { h1Atts: oneOf
      [ style_ "color:blue;"
      , id_ := "my-h1"
      ]
  , txt: nut (D.span_ [ text_ " some html." ])
  }

main :: Effect Unit
main = runInBody app
