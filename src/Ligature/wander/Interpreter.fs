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

let rec evalExpression
    (hostFunctions: Map<string, Combinator>)
    (inputState: State)
    (expression: Expression)
    : Result<State, LigatureError> =
    match expression with
    | Expression.NetworkName name -> evalNetworkName name inputState
    | Expression.Network(network) -> evalNetwork inputState network
    | Expression.Call(name) -> handleWord inputState name

and handleWord (inputState: State) (word: Word) =
    let res =
        Set.filter
            (fun (e, a, v) ->
                match (e, a, v) with
                | (PatternWord.Word(entity), PatternWord.Word(attribute), LigatureValue.Pipeline(_)) ->
                    entity = word && attribute = Word("=")
                | (PatternWord.Word(entity), PatternWord.Word(attribute), LigatureValue.HostCombinator(_)) ->
                    entity = word && attribute = Word("=")
                | _ -> false)
            (currentNetwork inputState)

    match (word, List.ofSeq (res)) with
    | (Word(word), []) -> error $"Could not find Word, {word}" None
    | (_, [ (_, _, LigatureValue.Pipeline(quote)) ]) -> failwith "TODO" //evalQuote hostFunctions runtimeNetwork quote
    | (_, [ (_, _, LigatureValue.HostCombinator(combinator)) ]) -> combinator.Eval inputState
    | _ -> error $"Multiple matches found for Word, {word}" None

and evalExpressions
    (hostFunctions: Map<string, Combinator>)
    (inputState: State)
    (expressions: Expression list)
    : Result<State, LigatureError> =
    match expressions with
    | [] -> Ok(inputState)
    | [ head ] -> evalExpression hostFunctions inputState head
    | head :: tail ->
        match evalExpression hostFunctions inputState head with
        | Ok(res) -> evalExpressions hostFunctions res tail
        | Error(err) -> Error(err)

and valuesToExpressions
    (values: LigatureValue list)
    (expressions: Expression list)
    : Result<Expression list, LigatureError> =
    match values with
    | [] -> Ok(expressions)
    | head :: tail ->
        match head with
        | LigatureValue.Network n -> valuesToExpressions tail (List.append expressions [ Expression.Network n ])
        | LigatureValue.Word w ->
            match tail with
            | LigatureValue.Pipeline(q) :: tail -> failwith "TODO"
            | _ -> valuesToExpressions [] (List.append expressions [ Expression.Call(w) ])
        | _ -> error "Invalid Quote" None

and evalQuote
    (hostFunctions)
    (inputState: State)
    (names: string list)
    (values: LigatureValue list)
    : Result<State, LigatureError> =
    failwith "TODO"
