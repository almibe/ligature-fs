// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Main

open Ligature.Wander.Model
open Error


let run (input: string) (bindings: Bindings) =
    match Lexer.tokenize input with
    | Ok tokens ->
        match Parser.parse tokens with
        | Ok ast -> Interpreter.interpret ast bindings
        | Error _ -> error "Error parsing." None
    | Error _ -> error "Error tokenizing." None
