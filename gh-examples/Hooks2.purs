module Hooks2 where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf)
import Data.Tuple.Nested ((/\))
import Deku.Control (text)
import Deku.Core (Nut)
import Deku.DOM as D
import Deku.Do (useMemoized)
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
  <h1>A counter</h1>
  <p>Counter: ~counterTxt1~</p>
  <p>Counter: ~counterTxt2~</p>
  <p>Counter: ~counterTxt3~</p>
  <p>Counter: ~counterTxt4~</p>
  <p>Counter: ~counterTxt5~</p>
  <p>Counter: ~counterTxt6~</p>
  <p>Counter: ~counterTxt7~</p>
  <p>Counter: ~counterTxt8~</p>
  <p>Counter: ~counterTxt9~</p>
  <button ~button1~>Increment</button>
</div>
"""

app :: Nut
app = Deku.do
  setCounter /\ counter <- useMemoized
    ( \i -> show <$> (fold (const (add 1)) i 0)
        <|> pure "0"
    )
  let ctxt = nut (D.div_ [ text counter ])
  template ~~
    { counterTxt1: ctxt
    , counterTxt2: ctxt
    , counterTxt3: ctxt
    , counterTxt4: ctxt
    , counterTxt5: ctxt
    , counterTxt6: ctxt
    , counterTxt7: ctxt
    , counterTxt8: ctxt
    , counterTxt9: nut (text counter)
    , button1: oneOf [ click $ pure $ setCounter unit ]
    }

main :: Effect Unit
main = runInBody app