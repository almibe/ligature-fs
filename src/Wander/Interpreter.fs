// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Interpreter

open Ligature.Main
open Model

//let evalNetworkName (networks: LigatureStore) (name: NetworkName) : Result<Identifier, LigatureError> = Ok(networks)

let evalNetwork
    (store: LigatureEngine)
    (name: NetworkName)
    (network: Set<Entry>)
    : Result<Value option, LigatureError> =
    store.AddEntries name network |> ignore
    Ok None

let rec evalElement
    (commands: Commands)
    networks
    (arguments: Value list)
    (Element(name))
    : Result<Value option, LigatureError> =
    match commands.TryFind(Element(name)) with
    | Some(command) -> command.Eval commands networks arguments
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

and evalCalls (commands: Commands) store (calls: Call list) : Result<Value option, LigatureError> =
    match calls with
    | [] -> Ok(None)
    | [ head ] -> evalCall commands store head
    | head :: tail ->
        match evalCall commands store head with
        | Ok(value) -> evalCalls commands store tail
        | Error(err) -> Error(err)

and evalCall
    (commands: Commands)
    (store: LigatureEngine)
    ((name, args): Call)
    : Result<Value option, LigatureError> =
       evalElement commands store args name

and evalQuote
    (commands: Commands)
    (store: LigatureEngine)
    (quote: Quote)
    : Result<Value option, LigatureError> =
        match quote with
        | [] -> failwith "TODO"
        | [Value.Element name] -> evalElement commands store [] name
        | _ -> 
            match quote.Head with
            | Value.Element name -> evalElement commands store quote.Tail name
            | _ -> failwith "TODO"
