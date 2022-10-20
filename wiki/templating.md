# Templating

Deku uses a templating system for working with HTML. If you've used Handlebars, Mustache, Svelte, or JSX, you'll hopefully find Deku's templating familiar.

A basic Deku template document looks like this.

[Try Me](https://try.purescript.org/?github=/mikesol/purescript-deku-realworld/main/gh-examples/Pursx1.purs)
```purescript
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
```

There are two flavors of logic you can add to your template: _component_ logic, which adds attributes and listeners to components, and _child_ logic, which adds new child components. The following example does both.

[Try Me](https://try.purescript.org/?github=/mikesol/purescript-deku-realworld/main/gh-examples/Pursx2.purs)
```purescript
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
      [ pure $ D.Style := "color:blue;"
      , pure $ D.Id := "my-h1"
      ]
  , txt: nut (D.span_ [ text_ " some html." ])
  }

main :: Effect Unit
main = runInBody app
```

Of course, you won't get far without some sort of logic that makes your page more dynamic. Deku makes a page coming alive by using [Events](./events.md).
