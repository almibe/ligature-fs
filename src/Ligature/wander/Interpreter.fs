// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Interpreter

open Ligature.Wander.Model
open Ligature.Main

let evalNetwork (state: State) (network: Network) : Result<State, LigatureError> = Ok(Set.union state network)

let rec evalExpression (inputState: State) (expression: Expression) : Result<State, LigatureError> =
    match expression with
    | Expression.Network(network) -> evalNetwork inputState network
    | Expression.Call(name, arguments) -> handleCall inputState name arguments

and handleCall (state: State) (identifier: Name) (arguments: (Name * LigatureValue) list) =
    let lookupResult = readBinding (PatternName.Name identifier) state

    if lookupResult.IsSome then
        match lookupResult.Value with
        | LigatureValue.HostCombinator(combinator) -> combinator.Eval state arguments
        | LigatureValue.Quote(quote) -> failwith "TODO" //evalQuote hostFunctions runtimeNetwork quote
        | _ -> failwith "TODO"
    else
        error $"Could not find Name, {identifier}" None

and evalExpressions (inputState: State) (expressions: Expression list) : Result<State, LigatureError> =
    match expressions with
    | [] -> Ok(inputState)
    | [ head ] -> evalExpression inputState head
    | head :: tail ->
        match evalExpression inputState head with
        | Ok(res) -> evalExpressions res tail
        | Error(err) -> Error(err)

and valuesToExpressions
    (values: LigatureValue list)
    (expressions: Expression list)
    : Result<Expression list, LigatureError> =
    match values with
    | [] -> Ok expressions
    | head :: tail ->
        match head with
        | LigatureValue.Network n -> valuesToExpressions tail (List.append expressions [ Expression.Network n ])
        | LigatureValue.Name i ->
            match tail with
            | LigatureValue.Quote p :: tail -> failwith "TODO"
            | _ -> valuesToExpressions [] (List.append expressions [ Expression.Call(i, []) ])
        | _ -> error "Invalid Quote" None

and evalQuote
    (hostFunctions)
    (inputState: State)
    (names: string list)
    (values: LigatureValue list)
    : Result<State, LigatureError> =
    failwith "TODO"
