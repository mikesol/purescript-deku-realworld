# Hooks

Deku uses React-like hooks for creating local events in components. The `useState` hook, like React's `useState`, updates the UI whenever a value is changed. There is also a `useState'` function that does not require an initial value, and the UI changes only when the event is emitted.

[Try Me](https://try.purescript.org/?github=/mikesol/purescript-deku-realworld/main/gh-examples/Hooks1.purs)
```purescript
module Pursx2 where

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
  Proxy
    :: Proxy
         """
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
        [ color <#> \c -> D.Style := "style=\"color:" <> c <> ";\""
        , pure $ D.Id := "my-h1"
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
```

Sometimes, for performance reasons, you need to apply a transformation to an event only once and have that transformation be shared between multiple DOM elements. This is often the case, for example, when working with local state that feeds into several elements. In these cases, `useMemoized` is a useful tool.

In the example below, the fold and the `show` is only applied once, and all of the text nodes draw from them. For larger apps, this can lead to significant performance gains.

[Try me]()
```purescript
module Pursx2 where

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
  Proxy
    :: Proxy
         """
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
```

Dynamic elements, like lists, can be created with `dyn` and can be addressed with the `useMailboxed` hook.

[Try me]()
```purescript
```

A special case of dynamic elements is when one element replaces another. This is how routing is done, and for this case, there is a special function called `switcher`.

[Try me]()
```purescript
```

Outside of these patterns, Deku and this RealWorld application rely heavily on [standard functional programming conventions and standard PureScript libraries](./functional-programming).