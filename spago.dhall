{ sources = [ "./src/**/*.purs" ]
, name = "deku-starter"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "deku"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "hyrule"
  , "maybe"
  , "prelude"
  , "simple-json"
  , "web-html"
  ]
, packages = ./packages.dhall
}
