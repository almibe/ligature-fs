// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Cli

open Wander.Main
open Wander.Lib
open Wander.Model

[<EntryPoint>]
let main (args: string[]) =
    let dir = System.IO.Directory.GetCurrentDirectory()
    let file = $"{dir}/{args[0]}"
    let script = System.IO.File.ReadAllText(file)

    match run stdCommands emptyVariables script with
    | Ok(Some(res), _, _) -> printfn $"{(prettyPrint res)}"
    | Ok(None, _, _) -> printfn "--no result--"
    | Error err -> printfn $"{err.UserMessage}"

    0
