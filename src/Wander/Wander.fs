// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Main

open Ligature

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

let run (input: string) =
    match Lexer.tokenize input with
    | Ok(tokens) ->
        match Parser.parse tokens with
        | Ok ast -> Interpreter.interpret ast
        | Error _ -> error "Error parsing." None
    | Error _ -> error "Error tokenizing." None
