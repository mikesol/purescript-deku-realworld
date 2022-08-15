module Pursx1 where

import Prelude

import Deku.Core (Nut)
import Deku.Pursx ((~~))
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Type.Proxy (Proxy(..))

template = Proxy :: Proxy """
<div>
  <h1 style="color:blue;">Hello world!</h1>
  <p><i>This is</i> some HTML.</p>
</div>
"""

app :: Nut
app = template ~~ {}

main :: Effect Unit
main = runInBody app