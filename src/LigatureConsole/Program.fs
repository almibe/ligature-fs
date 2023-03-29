// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

open Ligature.Wander.Main
open Spectre.Console;

let mutable cont = true

while cont do
    let input = AnsiConsole.Ask<string>("?> ")
    match input with
    | ":quit" -> cont <- false
    | _ -> printfn "%A" (run input)
