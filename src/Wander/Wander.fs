// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Main

open Parser
open Tokenizer
open Ligature.Model
open Interpreter
open Wander.Model
open Library
open Ligature.InMemoryStore

let run (fns: Fns) (variables: Variables) (input: string) : Result<Expression, LigatureError> =
    try
        match tokenize input with
        | Ok tokens ->
            match parse tokens with
            | Ok script ->
                match evalScript fns variables script with
                | Ok res -> Ok res
                | Error err -> Error err
            | Error err -> error $"Error parsing.\n{err}" None
        | Error _ -> error "Error tokenizing." None
    with x ->
        error $"Error running script. {x}" None

let runWithDefaults (script: string) =
    run (stdFns (InMemoryStore())) Map.empty script

let printResult (result: Result<Expression, LigatureError>) =
    match result with
    | Ok res -> printExpression res
    | Error err -> $"Error {err.UserMessage}"
