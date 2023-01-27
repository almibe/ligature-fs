// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Main

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

let run (input: string) =
    match Parser.parse input with
    | Ok(ast) ->
        Interpreter.interpret ast
    | Error(err) -> Error(err)
