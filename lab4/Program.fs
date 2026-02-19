module WhatToCook.Program

open WhatToCook.Domain
open WhatToCook.Cli

[<EntryPoint>]
let main _argv =
    Cli.run defaultConfig
    0
