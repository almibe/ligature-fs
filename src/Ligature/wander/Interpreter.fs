// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Interpreter

open Ligature.Wander.Model
open Ligature.Main

let evalNetworkName ((_, networks): State) (name: NetworkName) : Result<State * LigatureValue option, LigatureError> =
    Ok((name, networks), None)

let evalNetwork ((name, networks): State) (network: Network) : Result<State * LigatureValue option, LigatureError> =
    let currentNetwork = currentNetwork (name, networks)
    let newNetwork = Set.union currentNetwork (network)
    let newNetworks = Map.add name newNetwork networks
    Ok((name, newNetworks), None)


let evalName
    (combinators: Combinators)
    (state: State)
    (arguments: LigatureValue list)
    (Name(name))
    : Result<State * LigatureValue option, LigatureError> =
    //TODO check state for bindings
    match combinators.TryFind (Name(name)) with
    | Some(combinataor) -> combinataor.Eval combinators state arguments
    | None -> 
        error $"Could not find name {name}" None

let rec evalElement
    (combinators: Combinators)
    (inputState: State)
    (element: Element)
    : Result<State * LigatureValue option, LigatureError> =
    match element with
    | Element.Network network -> evalNetwork inputState network
    | Element.Name name -> evalName combinators inputState [] name
    | Element.NetworkName name -> evalNetworkName inputState name
    | Element.Expression expression -> evalExpression combinators inputState expression

and evalElements
    (combinators: Combinators)
    (inputState: State)
    (elements: Element list)
    : Result<State * LigatureValue option, LigatureError> =
    match elements with
    | [] -> Ok(inputState, None)
    | [ head ] -> evalElement combinators inputState head
    | head :: tail ->
        match evalElement combinators inputState head with
        | Ok(res, value) -> evalElements combinators res tail
        | Error(err) -> Error(err)

and evalExpression
    (combinators: Combinators)
    (inputState: State)
    (expression: Expression)
    : Result<State * LigatureValue option, LigatureError> =
    match expression with
    | [] -> Ok(inputState, None)
    | [ LigatureValue.Name(name) ] -> evalName combinators inputState [] name
    | [ LigatureValue.NetworkName(name) ] -> evalNetworkName inputState name
    | LigatureValue.Name(name) :: tail -> evalName combinators inputState tail name
    | _ -> error "Invalid Quote." None
