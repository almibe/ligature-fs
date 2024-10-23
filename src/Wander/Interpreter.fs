// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Interpreter

open Ligature.Main

//let evalNetworkName (networks: LigatureStore) (name: NetworkName) : Result<Identifier, LigatureError> = Ok(networks)

let evalNetwork
    (store: LigatureStore)
    (name: NetworkName)
    (network: Set<Entry>)
    : Result<WanderValue option, LigatureError> =
    store.Add name network |> ignore
    Ok None

let rec evalSymbol
    (commands: Commands)
    networks
    (arguments: WanderValue list)
    (Symbol(name))
    : Result<WanderValue option, LigatureError> =
    let newArguments = processArguments commands networks arguments

    match commands.TryFind(Symbol(name)) with
    | Some(command) -> command.Eval commands networks newArguments
    | None -> error $"Could not find name {name}" None

and processArguments commands networks (arguments: WanderValue list) : WanderValue list =
    List.map
        (fun argument ->
            match argument with
            | WanderValue.Expression e ->
                match evalExpression commands networks e with
                | Ok(Some(value)) -> value
                | _ -> WanderValue.Network Set.empty
            | value -> value)
        arguments

and evalElement
    (commands: Commands)
    (store: LigatureStore)
    (element: WanderElement)
    : Result<WanderValue option, LigatureError> =
    match element with
    | WanderElement.Network(name, network) -> evalNetwork store name network
    | WanderElement.Expression expression -> evalExpression commands store expression

and evalElements
    (commands: Commands)
    store
    (elements: WanderElement list)
    : Result<WanderValue option, LigatureError> =
    match elements with
    | [] -> Ok(None)
    | [ head ] -> evalElement commands store head
    | head :: tail ->
        match evalElement commands store head with
        | Ok(value) -> evalElements commands store tail
        | Error(err) -> Error(err)

and evalExpression
    (commands: Commands)
    (store: LigatureStore)
    (expression: Expression)
    : Result<WanderValue option, LigatureError> =
    match expression with
    | [] -> Ok(None)
    | [ WanderValue.Symbol(name) ] -> evalSymbol commands store [] name
    | WanderValue.Symbol(name) :: tail -> evalSymbol commands store tail name
    | _ -> error "Invalid Quote." None
