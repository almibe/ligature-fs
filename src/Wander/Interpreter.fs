// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Interpreter

open Ligature.Main

//let evalNetworkName (networks: LigatureStore) (name: NetworkName) : Result<Identifier, LigatureError> = Ok(networks)

let evalNetwork (store: LigatureStore) (name: Symbol) (network: ABox) : Result<WanderValue option, LigatureError> =
    store.Add name network |> ignore
    Ok None
// let newNetwork = Set.union currentNetwork (network)
// let newNetworks = Map.add name newNetwork networks
// Ok None

let rec evalSymbol
    (combinators: Combinators)
    networks
    (arguments: WanderValue list)
    (Symbol(name))
    : Result<WanderValue option, LigatureError> =
    let newArguments = processArguments combinators networks arguments

    match combinators.TryFind(Symbol(name)) with
    | Some(combinator) -> combinator.Eval combinators networks newArguments
    | None -> error $"Could not find name {name}" None

and processArguments combinators networks (arguments: WanderValue list) : WanderValue list =
    List.map
        (fun argument ->
            match argument with
            | WanderValue.Expression e ->
                match evalExpression combinators networks e with
                | Ok(Some(value)) -> value
                | _ -> WanderValue.Network Set.empty
            | value -> value)
        arguments

and evalElement
    (combinators: Combinators)
    (store: LigatureStore)
    (element: WanderElement)
    : Result<WanderValue option, LigatureError> =
    match element with
    | WanderElement.Network(name, network) -> evalNetwork store name network
    | WanderElement.Expression expression -> evalExpression combinators store expression

and evalElements
    (combinators: Combinators)
    store
    (elements: WanderElement list)
    : Result<WanderValue option, LigatureError> =
    match elements with
    | [] -> Ok(None)
    | [ head ] -> evalElement combinators store head
    | head :: tail ->
        match evalElement combinators store head with
        | Ok(value) -> evalElements combinators store tail
        | Error(err) -> Error(err)

and evalExpression
    (combinators: Combinators)
    (store: LigatureStore)
    (expression: Expression)
    : Result<WanderValue option, LigatureError> =
    match expression with
    | [] -> Ok(None)
    | [ WanderValue.Symbol(name) ] -> evalSymbol combinators store [] name
    | WanderValue.Symbol(name) :: tail -> evalSymbol combinators store tail name
    | _ -> error "Invalid Quote." None
