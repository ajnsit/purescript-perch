{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "perch"
, dependencies =
  [ "console"
  , "control"
  , "effect"
  , "foldable-traversable"
  , "prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
