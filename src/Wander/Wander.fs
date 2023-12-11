// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Main

open Ligature.Wander.Model
open Error
open Parser
open Lexer

let run (input: string) (bindings: Bindings) =
    match Lexer.tokenize input with
    | Ok tokens ->
        match parse tokens with
        | Ok ast ->
            let expressions = List.map (fun element -> express element) ast
            Result.map (fun (res, _) -> res) (Interpreter.evalExpressions bindings expressions)
        | Error _ -> error "Error parsing." None
    | Error _ -> error "Error tokenizing." None

type Introspect = {
    tokens: Result<Token list, string>
    elements: Result<Element list, string>
}

let introspect (input: string) =
    match Lexer.tokenize input with
    | Ok tokens -> 
        match parse tokens with
        | Ok value -> { tokens = Ok tokens; elements = Ok value }
        | Error err -> { tokens = Ok tokens; elements = Error (string err) }
    | Error err -> { tokens = Error (string err); elements = Error (string err) }

let printResult (result: Result<WanderValue, WanderError>) =
    match result with
    | Ok value -> prettyPrint value
    | Error err -> err.UserMessage