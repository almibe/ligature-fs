// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Main

open Ligature

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

let run (input: string) =
    match Wander.Lexer.tokenize input with
    | Ok(tokens) ->
        match Parser.parse tokens with
        | Ok(ast) -> Interpreter.interpret ast
        | Error(err) -> Error(err)
    | Error(err) -> Error(err)
    