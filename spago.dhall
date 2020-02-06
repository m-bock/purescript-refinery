{ name =
    "refinery"
, dependencies =
    [ "console"
    , "effect"
    , "either"
    , "generics-rep"
    , "psci-support"
    , "strings"
    , "these"
    , "typelevel"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" , "example/**/*.purs"]
}
