// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Interpreter

open Ligature.Main
open Model

let rec evalElement
    (commands: Commands)
    (variables: Variables)
    (arguments: Value list)
    (Element(name))
    : Result<Value option, LigatureError> =
    match commands.TryFind(Element(name)) with
    | Some(command) -> command.Eval commands variables arguments
    | None -> error $"Could not find name {name}" None

and processArguments commands networks (arguments: Value list) : Value list =
    List.map
        (fun argument ->
            match argument with
            | Value.Quote quote ->
                match evalQuote commands networks quote with
                | Ok(Some(value)) -> value
                | _ -> Value.Network Set.empty
            | value -> value)
        arguments

and evalCalls (commands: Commands) (variables: Variables) (calls: Call list) : Result<Value option, LigatureError> =
    match calls with
    | [] -> Ok(None)
    | [ head ] -> evalCall commands variables head
    | head :: tail ->
        match evalCall commands variables head with
        | Ok(value) -> evalCalls commands variables tail
        | Error(err) -> Error(err)

and evalCall (commands: Commands) (variables: Variables) ((name, args): Call) : Result<Value option, LigatureError> =
    evalElement commands variables args name

and evalQuote (commands: Commands) (variables: Variables) (quote: Quote) : Result<Value option, LigatureError> =
    match quote with
    | [] -> failwith "TODO"
    | [ Value.Element name ] -> evalElement commands variables [] name
    | _ ->
        match quote.Head with
        | Value.Element name -> evalElement commands variables quote.Tail name
        | _ -> failwith "TODO"
