{ sources = [ "./src/**/*.purs" ]
, name = "deku-starter"
, dependencies =
  [ "deku"
  , "effect"
  , "hyrule"
  , "foldable-traversable"
  , "prelude"
  , "web-html"
  ]
, packages = ./packages.dhall
}
