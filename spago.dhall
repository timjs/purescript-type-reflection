{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "type-reflection"
, dependencies =
  [ "prelude"
  , "generics-rep"
  , "unsafe-coerce"
  , "type-equality"
  , "proxy"
  , "psci-support"
  ]
, packages = ./packages.dhall
}
