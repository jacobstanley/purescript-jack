{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "jack"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "foreign-object"
  , "generics-rep"
  , "int-53"
  , "lists"
  , "prelude"
  , "psci-support"
  , "random"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
