// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Interpreter

open Ligature.Main
open Ligature.InMemoryNetwork

let evalNetworkName (networks: Networks) (name: NetworkName) : Result<State, LigatureError> = Ok(name, networks, None)

let evalNetwork name networks (network: Network) : Result<State, LigatureError> =
    let currentNetwork = currentNetwork name networks
    let newNetwork = Set.union currentNetwork (network)
    let newNetworks = Map.add name newNetwork networks
    Ok(name, newNetworks, None)

let rec evalName
    (combinators: Combinators)
    networkName
    networks
    (arguments: LigatureValue list)
    (Name(name))
    : Result<State, LigatureError> =
    let newArguments = processArguments combinators networkName networks arguments

    match combinators.TryFind(Name(name)) with
    | Some(combinataor) -> combinataor.Eval combinators networkName networks newArguments
    | None -> error $"Could not find name {name}" None

and processArguments combinators networkName networks (arguments: LigatureValue list) : LigatureValue list =
    List.map
        (fun argument ->
            match argument with
            | LigatureValue.Expression e ->
                match evalExpression combinators networkName networks e with
                | Ok(_, _, Some(value)) -> value
                | _ -> LigatureValue.Network emptyNetwork
            | value -> value)
        arguments

and evalElement (combinators: Combinators) networkName networks (element: Command) : Result<State, LigatureError> =
    match element with
    | Command.Network network -> evalNetwork networkName networks network
    | Command.NetworkName name -> evalNetworkName networks name
    | Command.Expression expression -> evalExpression combinators networkName networks expression

and evalElements
    (combinators: Combinators)
    networkName
    networks
    (elements: Command list)
    : Result<State, LigatureError> =
    match elements with
    | [] -> Ok(networkName, networks, None)
    | [ head ] -> evalElement combinators networkName networks head
    | head :: tail ->
        match evalElement combinators networkName networks head with
        | Ok(resName, resNetworks, value) -> evalElements combinators resName resNetworks tail
        | Error(err) -> Error(err)

and evalExpression
    (combinators: Combinators)
    (networkName: NetworkName)
    (networks: Networks)
    (expression: Expression)
    : Result<State, LigatureError> =
    match expression with
    | [] -> Ok(networkName, networks, None)
    | [ LigatureValue.Name(name) ] -> evalName combinators networkName networks [] name
    | [ LigatureValue.NetworkName(name) ] -> evalNetworkName networks name
    | LigatureValue.Name(name) :: tail -> evalName combinators networkName networks tail name
    | _ -> error "Invalid Quote." None
