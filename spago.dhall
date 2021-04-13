{ name = "halogen-project"
, dependencies =
  [ "aff"
  , "aff-coroutines"
  , "argonaut"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "halogen"
  , "halogen-css"
  , "js-timers"
  , "node-fs-aff"
  , "now"
  , "numbers"
  , "psci-support"
  , "web-socket"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
