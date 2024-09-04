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

let run (combinators: Combinators) ((networkName, networks, _): State) (input: string) : Result<State, LigatureError> =
    try
        match tokenize input with
        | Ok tokens ->
            match parse tokens with
            | Ok ast -> express ast [] |> evalElements combinators networkName networks
            | Error(err) -> error $"Error parsing.\n{err}" None
        | Error _ -> error "Error tokenizing." None
    with x ->
        failwith $"{x}"
//error $"Error running script. {x}" None

type Introspect =
    { tokens: Result<Token list, string>
      elements: Result<Command list, string>
      expressions: Result<Command list, string> }

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
    | Ok(name, state, _) -> $"@{name} {printNetwork (currentNetwork name state)}"
    | Error err -> err.UserMessage

type WanderEngine =
    abstract Run: script: string -> int
    abstract ReadResult: id: int -> Result<LigatureValue option, LigatureError> option
    abstract ReadScript: id: int -> string option

type InMemoryWanderEngine(combinators: Combinators) =
    let mutable combinators = combinators
    let mutable state = defaultState
    let mutable resultId = 0

    let mutable results: Map<int, Result<LigatureValue option, LigatureError>> =
        Map.empty

    let mutable scripts: Map<int, string> = Map.empty

    interface WanderEngine with
        member _.Run(script: string) : int =
            match run combinators state script with
            | Ok res ->
                state <- res
                let (_, _, res) = res
                let id = resultId
                resultId <- resultId + 1
                scripts <- Map.add id script scripts
                results <- Map.add id (Ok res) results
                id
            | Error err -> failwith "TODO"

        member _.ReadResult(id: int) : Result<LigatureValue option, LigatureError> option = results.TryFind id
        member _.ReadScript(id: int) : string option = scripts.TryFind id
