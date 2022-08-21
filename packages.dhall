let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20220808/packages.dhall
        sha256:60eee64b04ca0013fae3e02a69fc3b176105c6baa2f31865c67cd5f881a412fd

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
      , fetch-core =
        { dependencies =
          [ "arraybuffer-types"
          , "arrays"
          , "console"
          , "effect"
          , "foldable-traversable"
          , "foreign"
          , "foreign-object"
          , "functions"
          , "http-methods"
          , "maybe"
          , "newtype"
          , "nullable"
          , "prelude"
          , "record"
          , "tuples"
          , "typelevel-prelude"
          , "unfoldable"
          , "unsafe-coerce"
          , "web-file"
          , "web-promise"
          , "web-streams"
          ]
        , version = "v4.0.4"
        , repo = "https://github.com/rowtype-yoga/purescript-fetch-core.git"
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
        , version = "v1.0.0"
        , repo = "https://github.com/rowtype-yoga/purescript-fetch.git"
        }
      , fetch-yoga-json =
        { dependencies =
          [ "aff"
          , "either"
          , "exceptions"
          , "foreign"
          , "lists"
          , "prelude"
          , "transformers"
          , "yoga-json"
          ]
        , version = "v1.0.1"
        , repo = "https://github.com/rowtype-yoga/fetch-yoga-json.git"
        }
      }

in  upstream // overrides
