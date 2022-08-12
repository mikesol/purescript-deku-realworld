module Components.Footer where

import Prelude

import Deku.Core (Nut)
import Deku.Pursx ((~~))
import Type.Proxy (Proxy(..))

footer_ =
  Proxy :: Proxy """<footer>
    <div class="container">
        <a href="/" class="logo-font">conduit</a>
        <span class="attribution">
          An interactive learning project from <a href="https://thinkster.io">Thinkster</a>. Code &amp; design licensed under MIT.
        </span>
    </div>
</footer>
"""

footer :: Nut
footer = footer_ ~~ {}