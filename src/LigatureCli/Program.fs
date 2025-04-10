﻿// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Cli

open Wander.Main
open Wander.Library
open Wander.Model
open Wander.InMemoryStore

[<EntryPoint>]
let main (args: string[]) =
    let dir = System.IO.Directory.GetCurrentDirectory()
    let file = $"{dir}/{args[0]}"
    let script = System.IO.File.ReadAllText file

    match run (stdFns (new InMemoryStore())) Map.empty Map.empty script with
    | Ok res -> printfn $"{printAny res}"
    | Error err -> printfn $"{err.UserMessage}"

    0
