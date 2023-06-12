# Events

Events are the bread and butter of Deku communication. If you've used Rx or any reactive framework, events in Deku shoul feel familiar. An emitter emits events, a subscriber listens to them, and various functions are used to transform and combine events.

In the example below, we use an `interval` emitter, which emits events every _n_ milliseconds, to update the value of a text field.

[Try Me](https://try.purescript.org/?github=/mikesol/purescript-deku-realworld/main/gh-examples/Events1.purs)
```purescript
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
  Proxy :: Proxy """
<div>
  <h1 ~h1Atts~>Hello world!</h1>
  <p><i>This is</i>~txt~</p>
</div>
"""

app :: Nut
app = template ~~
  { h1Atts: oneOf
      [ style_ "style=\"color:blue;\""
      , id_ := "my-h1"
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
```

`Event` is defined in [`purescript-hyrule`]() and it has many useful instances. Above, we see `Event`'s [`Functor`]() instance in action, which is where we get [`<#>`]() from. `Event` has an instance of a special typeclass called `IsEvent`. `IsEvent` deals mostly with operations over time. For example, `fold` allows you to fold over iterations of an event, which is how state is handled in Deku.

[Try Me](https://try.purescript.org/?github=/mikesol/purescript-deku-realworld/main/gh-examples/Events2.purs)
```purescript
module Events2 where

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
  Proxy :: Proxy """
<div>
  <h1 ~h1Atts~>Hello world!</h1>
  <p><i>This is</i>~txt~</p>
</div>
"""

app :: Nut
app = template ~~
  { h1Atts: oneOf
      [ style_ "style=\"color:blue;\""
      , id_ := "my-h1"
      ]
  , txt: nut
      ( D.span_
          [ text
              ( fromEvent
                  $ (fold (const (add 1)) (interval 100) 0 <|> pure 0) <#> (("Counting up: " <> _) <<< show)
              )
          ]
      )
  }

main :: Effect Unit
main = runInBody app
```

`Event` also has instances for `Apply` and `Applicative`, allowing you to use `<*>` and `pure`, as in the example below.

[Try Me](https://try.purescript.org/?github=/mikesol/purescript-deku-realworld/main/gh-examples/Events3.purs)
```purescript
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
  Proxy :: Proxy """
<div>
  <h1 ~h1Atts~>Hello world!</h1>
  <p><i>This is</i>~txt~</p>
</div>
"""

app :: Nut
app = template ~~
  { h1Atts: oneOf
      [ style_ "style=\"color:blue;\""
      , id_ := "my-h1"
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
```

There are many more useful functions and combinators in the [`purescript-hyrule`](https://github.com/mikesol/purescript-hyrule) library to work with events.

In web applications, events are mainly used to capture interactions with UI elements. Deku has a [hooks-inspired syntax](./hooks.md) for creating these interactions.