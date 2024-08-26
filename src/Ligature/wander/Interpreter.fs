// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Interpreter

open Ligature.Wander.Model
open Ligature.Main

let evalNetworkName (name: NetworkName) ((_, networks): State) : Result<State, LigatureError> = Ok(name, networks)

let evalNetwork ((name, networks): State) (network: Network) : Result<State, LigatureError> =
    let currentNetwork = currentNetwork (name, networks)
    let newNetwork = Set.union currentNetwork (network)
    let newNetworks = Map.add name newNetwork networks
    Ok(name, newNetworks)

//let evalNetwork (state: State) (network: Network) : Result<State, LigatureError> = Ok(Set.union state network)

// and handleCall (state: State) (identifier: Name) (arguments: (Name * LigatureValue) list) =
//     let lookupResult = readBinding (PatternName.Name identifier) state

//     if lookupResult.IsSome then
//         match lookupResult.Value with
//         | LigatureValue.HostCombinator(combinator) -> combinator.Eval state arguments
//         | LigatureValue.Quote(quote) -> failwith "TODO" //evalQuote hostFunctions runtimeNetwork quote
//         | _ -> failwith "TODO"
//     else
//         error $"Could not find Name, {identifier}" None

let evalName (combinators: Combinators) (state: State) (name: Name) : Result<State, LigatureError> =
    //TODO check state for bindings
    match combinators.TryFind name with
    | Some(combinataor) -> combinataor.Eval combinators state
    | None -> error $"Could not find name {name}." None

let rec evalElement (combinators: Combinators) (inputState: State) (element: Element) : Result<State, LigatureError> =
    match element with
    | Element.Network network -> evalNetwork inputState network
    | Element.Name name -> evalName combinators inputState name
    | Element.NetworkName name -> evalNetworkName name inputState
    | Element.Quote pipeline -> evalQuote combinators inputState pipeline

and evalElements
    (combinators: Combinators)
    (inputState: State)
    (elements: Element list)
    : Result<State, LigatureError> =
    match elements with
    | [] -> Ok(inputState)
    | [ head ] -> evalElement combinators inputState head
    | head :: tail ->
        match evalElement combinators inputState head with
        | Ok(res) -> evalElements combinators res tail
        | Error(err) -> Error(err)

// and valuesToExpressions
//     (values: LigatureValue list)
//     (expressions: Expression list)
//     : Result<Expression list, LigatureError> =
//     match values with
//     | [] -> Ok expressions
//     | head :: tail ->
//         match head with
//         | LigatureValue.Network n -> valuesToExpressions tail (List.append expressions [ Expression.Network n ])
//         | LigatureValue.Name i ->
//             match tail with
//             | LigatureValue.Quote p :: tail -> failwith "TODO"
//             | _ -> failwith "TODO" //valuesToExpressions [] (List.append expressions [ Expression.Call(i, []) ])
//         | _ -> error "Invalid Quote" None

and evalQuote (combinators: Combinators) (inputState: State) (pipeline: Quote) : Result<State, LigatureError> =
    failwith "TODO"
//evalElements combinators inputState pipeline.values
