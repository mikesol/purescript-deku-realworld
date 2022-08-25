let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20220822/packages.dhall
        sha256:908b4ffbfba37a0a4edf806513a555d0dbcdd0cde7abd621f8d018d2e8ecf828

let overrides =
      { bolson =
        { dependencies = [ "prelude", "fast-vect", "heterogeneous" ]
        , repo = "https://github.com/mikesol/purescript-bolson.git"
        , version = "main"
        }
      , deku =
        { dependencies = [ "prelude", "bolson", "quickcheck" ]
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
       , fetch =
         { dependencies =
           [ "aff"
           , "aff-promise"
           , "arraybuffer-types"
           , "effect"
           , "fetch-core"
           , "foreign"
           , "http-methods"
           , "newtype"
           , "prelude"
           , "record"
           , "typelevel-prelude"
           , "unsafe-coerce"
           , "web-file"
           , "web-promise"
           , "web-streams"
           ]
         , version = "v1.1.0"
         , repo = "https://github.com/rowtype-yoga/purescript-fetch.git"
         }
      }

in  upstream // overrides
