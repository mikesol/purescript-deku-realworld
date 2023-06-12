let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.7-20230127/packages.dhall
        sha256:e0063f83308aa72cffe25444fe86fb484341496c099e38b64574cc7440768fdd

let overrides =
      { bolson =
        { dependencies = [ "prelude", "fast-vect", "heterogeneous" ]
        , repo = "https://github.com/mikesol/purescript-bolson.git"
        , version = "main"
        }
      , deku =
        { dependencies = [ "prelude", "bolson", "catenable-lists", "css", "quickcheck" ]
        , repo = "https://github.com/mikesol/purescript-deku.git"
        , version = "main"
        }
      , hyrule =
        { dependencies =
          [ "monoid-extras"
          , "filterable"
          , "web-uievents"
          , "js-timers"
          , "unsafe-reference"
          ]
        , repo = "https://github.com/mikesol/purescript-hyrule.git"
        , version = "master"
        }
      }

in  upstream // overrides
