{ sources = [ "./src/**/*.purs" ]
, name = "deku-starter"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "arrays"
  , "bolson"
  , "control"
  , "debug"
  , "deku"
  , "effect"
  , "either"
  , "filterable"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "http-methods"
  , "hyrule"
  , "maybe"
  , "media-types"
  , "prelude"
  , "record"
  , "routing"
  , "routing-duplex"
  , "simple-json"
  , "st"
  , "stringutils"
  , "transformers"
  , "tuples"
  , "validation"
  , "web-dom"
  , "web-html"
  , "web-storage"
  ]
, packages = ./packages.dhall
}
