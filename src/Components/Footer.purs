module Components.Footer where

import Deku.Core (Nut)
import Deku.Pursx (pursx)

footer :: Nut
footer = pursx
  @"""<footer>
    <div class="container">
        <a href="/" class="logo-font">conduit</a>
        <span class="attribution">
          An interactive learning project from <a href="https://thinkster.io">Thinkster</a>. Code &amp; design licensed under MIT.
        </span>
    </div>
</footer>
"""
  {}