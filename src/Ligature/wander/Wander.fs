// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Main

open Ligature.Wander.Model
open Parser
open Lexer
open Ligature.Main
open Interpreter

let std: Map<string, Combinator> = Map.empty

let run
    (combinators: Combinators)
    (inputState: State)
    (input: string)
    : Result<State * LigatureValue option, LigatureError> =
    try
        match tokenize input with
        | Ok tokens ->
            match parse tokens with
            | Ok ast -> express ast [] |> evalElements combinators inputState
            | Error(err) -> error $"Error parsing.\n{err}" None
        | Error _ -> error "Error tokenizing." None
    with x ->
        failwith $"{x}"
//error $"Error running script. {x}" None

type Introspect =
    { tokens: Result<Token list, string>
      elements: Result<Element list, string>
      expressions: Result<Element list, string> }

let introspect (input: string) =
    match tokenize input with
    | Ok tokens ->
        match parse tokens with
        | Ok elements ->
            let expressions = failwith "TODO" //express elements []

            { tokens = Ok tokens
              elements = failwith "TODO" //Ok elements
              expressions = Ok expressions }
        | Error err ->
            { tokens = Ok tokens
              elements = Error(string err)
              expressions = Error(string err) }
    | Error err ->
        { tokens = Error(string err)
          elements = Error(string err)
          expressions = Error(string err) }

let printResult (result: Result<State, LigatureError>) =
    match result with
    | Ok(name, state) -> $"@{name} {printNetwork (currentNetwork (name, state))}"
    | Error err -> err.UserMessage
