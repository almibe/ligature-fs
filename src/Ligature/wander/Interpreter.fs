// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Interpreter

open Ligature.Main

//let evalNetworkName (networks: LigatureStore) (name: NetworkName) : Result<LigatureValue, LigatureError> = Ok(networks)

let evalNetwork (store: LigatureStore) (name: Symbol) (network: Network) : Result<LigatureValue option, LigatureError> =
    store.Add name network |> ignore
    Ok None
// let newNetwork = Set.union currentNetwork (network)
// let newNetworks = Map.add name newNetwork networks
// Ok None

let rec evalSymbol
    (combinators: Combinators)
    networks
    (arguments: LigatureValue list)
    (Symbol(name))
    : Result<LigatureValue option, LigatureError> =
    let newArguments = processArguments combinators networks arguments

    match combinators.TryFind(Symbol(name)) with
    | Some(combinator) -> combinator.Eval combinators networks newArguments
    | None -> error $"Could not find name {name}" None

and processArguments combinators networks (arguments: LigatureValue list) : LigatureValue list =
    List.map
        (fun argument ->
            match argument with
            | LigatureValue.Expression e ->
                match evalExpression combinators networks e with
                | Ok(Some(value)) -> value
                | _ -> LigatureValue.Network Set.empty
            | value -> value)
        arguments

and evalElement
    (combinators: Combinators)
    (store: LigatureStore)
    (element: Element)
    : Result<LigatureValue option, LigatureError> =
    match element with
    | Element.Network(name, network) -> evalNetwork store name network
    | Element.Expression expression -> evalExpression combinators store expression

and evalElements
    (combinators: Combinators)
    store
    (elements: Element list)
    : Result<LigatureValue option, LigatureError> =
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
    : Result<LigatureValue option, LigatureError> =
    match expression with
    | [] -> Ok(None)
    | [ LigatureValue.Symbol(name) ] -> evalSymbol combinators store [] name
    | LigatureValue.Symbol(name) :: tail -> evalSymbol combinators store tail name
    | _ -> error "Invalid Quote." None
