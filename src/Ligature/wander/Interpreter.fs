// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Interpreter

open Ligature.Main
open Ligature.InMemoryNetwork

//let evalNetworkName (networks: LigatureStore) (name: NetworkName) : Result<LigatureValue, LigatureError> = Ok(networks)

let evalNetwork (store: LigatureStore) (network: Network) : Result<LigatureValue option, LigatureError> =
    failwith "TODO"
// let currentNetwork = currentNetwork name networks
// let newNetwork = Set.union currentNetwork (network)
// let newNetworks = Map.add name newNetwork networks
// Ok(name, newNetworks, None)

let rec evalName
    (combinators: Combinators)
    networks
    (arguments: LigatureValue list)
    (Name(name))
    : Result<LigatureValue option, LigatureError> =
    let newArguments = processArguments combinators networks arguments

    match combinators.TryFind(Name(name)) with
    | Some(combinator) -> combinator.Eval combinators networks newArguments
    | None -> error $"Could not find name {name}" None

and processArguments combinators networks (arguments: LigatureValue list) : LigatureValue list = failwith "TODO"
// List.map
//     (fun argument ->
//         match argument with
//         | LigatureValue.Expression e ->
//             match evalExpression combinators networkName networks e with
//             | Ok(_, _, Some(value)) -> value
//             | _ -> LigatureValue.Network emptyNetwork
//         | value -> value)
//     arguments

and evalElement
    (combinators: Combinators)
    (store: LigatureStore)
    (element: Command)
    : Result<LigatureValue option, LigatureError> =
    match element with
    | Command.Network network -> evalNetwork store network
    | Command.Expression expression -> evalExpression combinators store expression

and evalElements
    (combinators: Combinators)
    networks
    (elements: Command list)
    : Result<LigatureValue option, LigatureError> =
    match elements with
    | [] -> Ok(None)
    | [ head ] -> evalElement combinators networks head
    | head :: tail ->
        match evalElement combinators networks head with
        | Ok(value) -> failwith "TODO" //evalElements combinators resName resNetworks tail
        | Error(err) -> Error(err)

and evalExpression
    (combinators: Combinators)
    (store: LigatureStore)
    (expression: Expression)
    : Result<LigatureValue option, LigatureError> =
    match expression with
    | [] -> Ok(None)
    | [ LigatureValue.Name(name) ] -> evalName combinators store [] name
    | LigatureValue.Name(name) :: tail -> evalName combinators store tail name
    | _ -> error "Invalid Quote." None
