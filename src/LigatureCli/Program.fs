// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Cli

open Wander.Main
open Wander.Library
open Wander.Model
open Wander.InMemoryStore
open System

[<EntryPoint>]
let main (args: string[]) =
    let script =
        if args[0] = "-i" then
            Console.ReadLine()
        else
            let dir = IO.Directory.GetCurrentDirectory()
            let file = $"{dir}/{args[0]}"
            System.IO.File.ReadAllText file

    match run (stdFns (new InMemoryStore())) Map.empty Map.empty script with
    | Ok res -> printfn $"{printAny res}"
    | Error err -> printfn $"{err.UserMessage}"

    0
