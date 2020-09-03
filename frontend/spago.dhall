{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ backend =
    "pskt"
, name =
    "my-project"
, dependencies =
    [ "arrays"
    , "catenable-lists"
    , "console"
    , "debug"
    , "effect"
    , "free"
    , "psci-support"
    , "record"

    , "typelevel-prelude"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "../common/**/*.purs", "src/**/*.purs" ]
}