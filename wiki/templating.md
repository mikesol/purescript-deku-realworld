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

[Try Me]()
```purescript
```

Of course, you won't get far without some sort of logic that makes your page more dynamic. Deku makes a page coming alive by using [Events](./event.md).