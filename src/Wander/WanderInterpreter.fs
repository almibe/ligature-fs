// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Interpreter

open Ligature
open Ligature.Wander.Model

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

let eval expression =
    match expression with
    | Value(value) -> Ok(value)
    | _ -> error $"Could not eval {expression}" None

let interpret (ast: Expression list) =
    let results = ast |> List.map eval
    List.last results
