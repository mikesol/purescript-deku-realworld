{ sources = [ "./src/**/*.purs" ]
, name = "deku-starter"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "bolson"
  , "control"
  , "deku"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "hyrule"
  , "maybe"
  , "prelude"
  , "routing"
  , "routing-duplex"
  , "simple-json"
  , "tuples"
  , "web-html"
  ]
, packages = ./packages.dhall
}
