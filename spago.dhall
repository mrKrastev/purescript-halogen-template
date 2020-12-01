{ name = "halogen-project"
, dependencies =
  [ "console"
  , "control"
  , "datetime"
  , "effect"
  , "halogen"
  , "halogen-css"
  , "js-timers"
  , "node-fs-aff"
  , "now"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
