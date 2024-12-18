// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Interpreter

open Ligature.Model
open Model

let rec evalElement
    (commands: Commands)
    (variables: Variables)
    (arguments: Any list)
    (Element(name))
    : Result<Any option, LigatureError> =
    match commands.TryFind(Element(name)) with
    | Some(command) -> command.Eval commands variables arguments
    | None -> error $"Could not find name {name}" None

and processArguments commands networks (arguments: Any list) : Any list =
    List.map
        (fun argument ->
            match argument with
            | Any.Quote quote ->
                match evalQuote commands networks quote with
                | Ok(Some(value)) -> value
                | _ -> Any.Network Set.empty
            | value -> value)
        arguments

and evalScript (commands: Commands) (variables: Variables) (script: Script) : Result<Any option, LigatureError> =
    match script with
    | [] -> Ok None
    | [ Expression.Call head ] -> evalCall commands variables head
    | [ _ ] -> Ok None
    | Expression.Call head :: tail ->
        match evalCall commands variables head with
        | Ok _ -> evalScript commands variables tail
        | Error err -> Error err
    | Expression.AnyAssignment(variable, value) :: tail -> evalScript commands (Map.add variable value variables) tail
    | Expression.CallAssignment(variable, call) :: tail ->
        match evalCall commands variables call with
        | Ok(Some value) -> evalScript commands (Map.add variable value variables) tail
        | _ -> failwith "TODO"

and evalCall (commands: Commands) (variables: Variables) ((name, args): Call) : Result<Any option, LigatureError> =
    evalElement commands variables args name

and evalQuote (commands: Commands) (variables: Variables) (quote: Quote) : Result<Any option, LigatureError> =
    match rewriteQuote quote with
    | Ok quote ->
        match quote with
        | [] -> failwith "TODO"
        | [ Any.Element name ] -> evalElement commands variables [] name
        | _ ->
            match quote.Head with
            | Any.Element name -> evalElement commands variables quote.Tail name
            | _ -> failwith "TODO"
    | _ -> failwith "TODO"

and rewriteQuote (quote: Quote) : Result<Quote, LigatureError> =
    let mutable quotes: List<List<Any>> = List.empty
    let mutable current: List<Any> = List.empty

    //chunk
    List.iter
        (fun item ->
            match item with
            | Any.Pipe ->
                quotes <- List.append quotes [ current ]
                current <- List.empty
            | value -> current <- List.append current [ value ])
        quote

    quotes <- List.append quotes [ current ]

    //process
    match quotes with
    | [] -> Ok []
    | [ single ] -> Ok single
    | _ ->
        let res =
            List.reduce (fun state item -> List.append item [ Any.Quote state ]) quotes

        Ok res
