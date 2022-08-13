let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20220808/packages.dhall
        sha256:60eee64b04ca0013fae3e02a69fc3b176105c6baa2f31865c67cd5f881a412fd

let overrides =
      { bolson =
        { dependencies = [ "prelude",  "fast-vect", "heterogeneous" ]
        , repo = "https://github.com/mikesol/purescript-bolson.git"
        , version = "main"
        }
      , deku =
        { dependencies = [ "prelude","bolson", "quickcheck" ]
        , repo = "https://github.com/mikesol/purescript-deku.git"
        , version = "no-span2"
        }
      , hyrule =
        { dependencies =
          [ "monoid-extras", "filterable", "web-uievents", "js-timers", "unsafe-reference" ]
        , repo = "https://github.com/mikesol/purescript-hyrule.git"
        , version = "master"
        }
      }

in  upstream // overrides
