// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Interpreter

open Ligature.Wander.Model
open Ligature.Main

let evalNetworkName (networks: Networks) (name: NetworkName) : Result<State, LigatureError> = Ok(name, networks, None)

let evalNetwork name networks (network: Network) : Result<State, LigatureError> =
    let currentNetwork = currentNetwork name networks
    let newNetwork = Set.union currentNetwork (network)
    let newNetworks = Map.add name newNetwork networks
    Ok(name, newNetworks, None)

let evalName
    (combinators: Combinators)
    networkName
    networks
    (arguments: LigatureValue list)
    (Name(name))
    : Result<State, LigatureError> =
    //TODO check state for bindings
    match combinators.TryFind(Name(name)) with
    | Some(combinataor) -> combinataor.Eval combinators networkName networks arguments
    | None -> error $"Could not find name {name}" None

let rec evalElement (combinators: Combinators) networkName networks (element: Element) : Result<State, LigatureError> =
    match element with
    | Element.Network network -> evalNetwork networkName networks network
    | Element.NetworkName name -> evalNetworkName networks name
    | Element.Expression expression -> evalExpression combinators networkName networks expression

and evalElements
    (combinators: Combinators)
    networkName
    networks
    (elements: Element list)
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
