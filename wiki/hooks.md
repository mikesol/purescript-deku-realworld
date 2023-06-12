# Hooks

Deku uses React-like hooks for creating local events in components. The `useState` hook, like React's `useState`, updates the UI whenever a value is changed. There is also a `useState'` function that does not require an initial value, and the UI changes only when the event is emitted.

[Try Me](https://try.purescript.org/?github=/mikesol/purescript-deku-realworld/main/gh-examples/Hooks1.purs)
```purescript
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
        [ color <#> \c -> D.Style := "style=\"color:" <> c <> ";\""
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
```

Sometimes, for performance reasons, you need to apply a transformation to an event only once and have that transformation be shared between multiple DOM elements. This is often the case, for example, when working with local state that feeds into several elements. In these cases, `useMemoized` is a useful tool.

In the example below, the fold and the `show` is only applied once, and all of the text nodes draw from them. For larger apps, this can lead to significant performance gains.

[Try Me](https://try.purescript.org/?github=/mikesol/purescript-deku-realworld/main/gh-examples/Hooks2.purs)
```purescript
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
```

Dynamic elements, like lists, can be created with `dyn` and can be addressed with the `useMailboxed` hook. Removal of dynamic elements is accomplished with the `useRemoval` hook.

[Try Me](https://try.purescript.org/?github=/mikesol/purescript-deku-realworld/main/gh-examples/Hooks3.purs)
```purescript
module Hooks3 where

import Prelude

import Control.Alt ((<|>))
import Data.Foldable (oneOf)
import Data.Int (floor)
import Data.Profunctor (lcmap)
import Data.Tuple.Nested ((/\))
import Deku.Attribute ((:=))
import Deku.Control (dyn_, text_)
import Deku.Core (Nut, insert_)
import Deku.DOM as D
import Deku.Do (useMailboxed, useMemoized, useRemoval)
import Deku.Do as Deku
import Deku.Listeners (click, numeric)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Event (fold)

app :: Nut
app = Deku.do
  setMakeBold /\ makeBold <- useMailboxed
  setMakeNormal /\ makeNormal <- useMailboxed
  setCounter /\ counter <- useMemoized
    ( \i -> fold (const (add 1)) i 0
        <|> pure 0
    )
  let asMailboxed = lcmap (floor >>> { address: _, payload: unit })
  D.div_
    [ D.div_
        [ D.label_ [ text_ "Make bold" ]
        , D.input (numeric $ pure (asMailboxed setMakeBold)) []
        , D.label_ [ text_ "Remove bold" ]
        , D.input (numeric $ pure (asMailboxed setMakeNormal)) []
        ]
    , D.div_ [ D.button (click (pure $ setCounter unit)) [ text_ "Add row" ] ]
    , dyn_
        D.div
        ( counter <#> \i -> Deku.do
            setRemoveMe /\ removeMe <- useRemoval
            pure
              ( insert_
                  ( D.div_
                      [ D.button
                          ( oneOf
                              [ click $ pure setRemoveMe
                              , oneOf
                                  [ makeBold i $> true
                                  , makeNormal i $> false
                                  ] <#> \tf -> D.Style := if tf then "font-weight:900;" else ""
                              ]
                          )
                          [ text_ "Remove" ]
                      , D.span_ [ text_ (show i) ]
                      ]
                  )
              ) <|> removeMe
        )
    ]

main :: Effect Unit
main = runInBody app
```

A special case of dynamic elements is when one element replaces another. This is how routing is done, and for this case, there is a special function called `switcher`.

[Try Me](https://try.purescript.org/?github=/mikesol/purescript-deku-realworld/main/gh-examples/Hooks4.purs)
```purescript
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
        B -> D.a
          ( pure
              $ D.Href := "https://github.com/mikesol/purescript-deku"
          )
          [ text_ "Page B navigates to the Deku Github Repo." ]
        C -> D.button_ [ text_ "Page C is a button that does nothing" ]
        D -> D.div_ [ D.h1_ [ text_ "Inception!", D.div_ [ app ] ] ]
    ]

main :: Effect Unit
main = runInBody app
```

Outside of these patterns, Deku and this RealWorld application rely heavily on [standard functional programming conventions and standard PureScript libraries](./functional-programming.md).