// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Main

open Ligature.Wander.Model
open Parser
open Lexer
open Ligature.Main

let run (input: string) (bindings: Bindings) =
    //try
    match tokenize input with
    | Ok tokens ->
        match parse tokens with
        | Ok ast ->
            let expressions = express ast
            Result.map (fun (res, _) -> res) (Interpreter.evalExpressions bindings expressions)
        | Error(err) -> error $"Error parsing.\n{err}" None
    | Error _ -> error "Error tokenizing." None
//with
//| x -> error $"Error running script. {x}" None

type Introspect =
    { tokens: Result<Token list, string>
      elements: Result<Element list, string>
      expressions: Result<Expression list, string> }

let introspect (input: string) =
    match tokenize input with
    | Ok tokens ->
        match parse tokens with
        | Ok elements ->
            let expressions = express elements

            { tokens = Ok tokens
              elements = Ok elements
              expressions = Ok expressions }
        | Error err ->
            { tokens = Ok tokens
              elements = Error(string err)
              expressions = Error(string err) }
    | Error err ->
        { tokens = Error(string err)
          elements = Error(string err)
          expressions = Error(string err) }

let printResult (result: Result<WanderValue, LigatureError>) =
    match result with
    | Ok value -> prettyPrint value
    | Error err -> err.UserMessage
