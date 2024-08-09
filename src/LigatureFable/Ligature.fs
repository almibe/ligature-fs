// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

open Ligature.Main
open Ligature.Wander.Lib.Combinators
open Ligature.Wander.Main
open Fable.Core.JsInterop

let printResult (script: string) : string = run stdState script |> printResult

let run script =
    let res = createEmpty

    match run stdState script with
    | Ok(NetworkName(_), networks) -> res
    | Error err ->
        res?errpr <- err.UserMessage
        res
