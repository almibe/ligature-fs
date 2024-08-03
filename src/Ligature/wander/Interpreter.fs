// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Interpreter

open Ligature.Wander.Model
open Ligature.Main
open Ligature.InMemoryNetwork

let evalNetworkName (name: NetworkName) ((_, networks): State) : Result<State, LigatureError> = Ok(name, networks)

let evalNetwork ((name, networks): State) (network: Network) : Result<State, LigatureError> =
    match currentNetwork (name, networks) with
    | Ok res ->
        let newNetwork = Set.union res (network)
        let newNetworks = Map.add name newNetwork networks
        Ok(name, newNetworks)
    | Error err -> Error err

let rec evalExpression
    (hostFunctions: Map<string, Combinator>)
    (inputState: State)
    (expression: Expression)
    : Result<State, LigatureError> =
    match expression with
    | Expression.NetworkName name -> evalNetworkName name inputState
    | Expression.Network(network) -> evalNetwork inputState network
    | Expression.Call(name) -> handleWord hostFunctions inputState name

// match value with
// | WanderValue.Word word -> handleWord words stack word
// | value -> Ok(value :: stack)

// and handleQuote (words: Words) (stack: Stack) (expressions: Expression list) =
//     let mutable error = None

//     let res: WanderValue list =
//         //TODO this doesn't short circuit on first error
//         List.map
//             (fun expr ->
//                 match expr with
//                 | Expression.Int value -> WanderValue.Int value
//                 | Expression.Word value -> WanderValue.Word value
//                 | Expression.Quote quote -> failwith "TODO" //WanderValue.Quote quote
//                 | Expression.Colon -> failwith "Not Implemented"
//                 | Expression.Bytes value -> WanderValue.Bytes value
//                 | Expression.String value -> WanderValue.String value
//                 | Expression.Identifier identifier -> WanderValue.Identifier identifier
//                 | Expression.Slot slot -> WanderValue.Slot slot
//                 | Expression.Definition(name, value) -> failwith "Not Implemented"
//                 | Expression.Quote(_) -> failwith "Not Implemented"
//                 | Expression.AssocArray assocArray -> failwith "TODO" //WanderValue.AssocArray assocArray
//                 | Expression.Network network -> failwith "TODO")
//             expressions

//     match error with
//     | None -> Ok([ WanderValue.Quote(res) ])
//     | Some(err) -> Error(err)

// let v = List.fold (fun state (name, value) -> Map.add name value state) (Map []) res
// Ok([WanderValue.AssocArray(v)])

// and handleEntityDescription words stack (attribute, values) : List<WanderValue> * List<List<WanderValue>> =
//     let attribute =
//         match evalExpression words stack attribute with
//         | Ok(res) -> res
//         | _ -> failwith "TODO"

//     let values =
//         List.map
//             (fun value ->
//                 match evalExpression words stack value with
//                 | Ok(res) -> res
//                 | _ -> failwith "TODO")
//             values

//     (attribute, values)

// and handleNetworkRootPattern (entity: Identifier) entityDescriptions =
//     let mutable triples: Set<Triple> = Set.empty

//     // let entity =
//     //     match evalExpression words stack entity with
//     //     | Ok([ res ]) -> res
//     //     | _ -> failwith "TODO"

//     // let (entityDescriptions: List<WanderValue * List<List<WanderValue>>>) =
//     //     List.map (fun entityDescription -> handleEntityDescription bindings entityDescription) entityDescriptions

//     List.iter
//         (fun entityDescription ->
//             let (attribute, values) = entityDescription

//             let entity =
//                 match entity with
//                 | WanderValue.Slot slot -> PatternWord.Sl slot
//                 | WanderValue.Identifier identifier -> PatternWord.Id identifier
//                 | _ -> failwith "TODO - entity"

//             let attribute =
//                 match attribute with
//                 | WanderValue.Slot slot -> PatternWord.Sl slot
//                 | WanderValue.Identifier identifier -> PatternWord.Id identifier
//                 | _ -> failwith "TODO - attribute"

//             List.iter
//                 (fun value ->
//                     let value =
//                         match value with
//                         | WanderValue.Int value -> Value.Int value
//                         | WanderValue.Bytes value -> Value.Bytes value
//                         | WanderValue.Identifier value -> Value.Identifier value
//                         | WanderValue.String value -> Value.String value
//                         | WanderValue.Slot slot -> Value.Slot(slot)
//                         | _ -> failwith "TODO - value"

//                     triples <-
//                         Set.add
//                             { Entity = entity
//                               Attribute = attribute
//                               Value = value }
//                             triples)
//                 values)
//             entityDescriptions
// //        (failwith "TODO")

//     Ok triples

// and handleNetwork (values: List<NetworkRoot>) =
//     let res = List.map (fun value -> handleNetworkRootPattern value) values
//     let mutable final: Set<Triple> = Set.empty

//     List.iter
//         (fun ds ->
//             match ds with
//             | Ok(res: Set<Triple>) -> final <- final + res
//             | _ -> failwith "TODO")
//         res

//     Ok([ WanderValue.Network(InMemoryNetwork(final)) ])

and handleWord (hostFunctions: Map<string, Combinator>) (inputState: State) (word: Word) =
    let res =
        Set.filter
            (fun (e, a, v) ->
                match (e, a, v) with
                | (PatternWord.Word(entity), PatternWord.Word(attribute), LigatureValue.Pipeline(_)) ->
                    entity = word && attribute = Word("=")
                // | (PatternWord.Word(entity), PatternWord.Word(attribute), LigatureValue.HostFunction(name)) ->
                //     entity = word && attribute = Word("=")
                | _ -> false)
            (match (currentNetwork inputState) with
             | Ok res -> res
             | _ -> failwith "TODO")
    // network.Query
    //     (networkOf ([ (PatternWord.Word(Word(word)), PatternWord.Word(Word("=")), Value.Slot(Slot(Some("name")))) ]))
    //     (networkOf ([ (PatternWord.Word(Word(word)), PatternWord.Word(Word("=")), Value.Slot(Slot(Some("name")))) ]))

    match (word, List.ofSeq (res)) with
    | (Word(word), []) ->
        match Map.tryFind word hostFunctions with
        | Some(res) -> failwith "TODO"
        | None ->

            error $"Could not find Word, {word}" None
    | (_, [ (_, _, LigatureValue.Pipeline(quote)) ]) -> failwith "TODO" //evalQuote hostFunctions runtimeNetwork quote
    | (_, [ (_, _, LigatureValue.Word(name)) ]) -> failwith "TODO"
    //         match Map.tryFind name hostFunctions with
    //         | Some(fn) ->
    //             fn.Eval runtimeNetwork args.quote
    // //            failwith "TODO"
    //         | None -> failwith "TODO"
    | _ -> error $"Multiple matches found for Word, {word}" None

// and evalHostFunction
//     (hostFunctions: Map<string, HostFunction>)
//     (runtimeNetwork: Network)
//     (hostFunction: HostFunction) =
//     hostFunction.Eval
//     failwith "TODO"
//bindings hostFunction arguments =
// let values =
//     List.map
//         (fun arg ->
//             match evalExpression bindings arg with
//             | Ok((value, _)) -> value
//             | Error(err) -> failwith $"Error calling Host Function: {err}")
//         arguments

// match hostFunction.Run values bindings with
// | Ok(res) -> Ok(res, bindings)
// | Error(err) -> Error(err)

// and checkPattern bindings (input: Network) (pattern: DatasetPatternRoot list) : bool =
//     //NOTE: calling evalExpression below is wrong since it will eval any names used for pattern matching
//     //this only works for matching with literals and no destructuring
//     match evalExpression bindings (Expression.Pattern(pattern)) with
//     | Ok(WanderValue.Network(pattern), _) -> failwith "TODO"
//     // match (pattern.AllTriples (), input.AllTriples ()) with
//     // | (Ok(pattern), Ok(triples)) -> Set.isSubset (Set.ofList pattern) (Set.ofList triples)
//     // | _ -> failwith "Error"
//     | _ -> failwith "Error"

// and handleQuery bindings inputExpression patterns =
//     let mutable results = emptyInMemoryNetwork

//     match evalExpression bindings inputExpression with
//     | Ok(input, _) ->
//         match input with
//         | WanderValue.Network(input) ->
//             List.iter
//                 (fun (pattern, body) ->
//                     match pattern with
//                     | Expression.Pattern(pattern) ->
//                         if checkPattern bindings input pattern then
//                             match evalExpression bindings body with
//                             | Ok(WanderValue.Dataset(dataset), _) ->
//                                 failwith "TODO"
//                                 // match dataset.AllTriples () with
//                                 // | Ok(triples) -> results <- InMemoryNetwork (results.triples + (Set.ofList triples))
//                                 // | _ -> failwith "Error reading Triples"
//                             | _ -> failwith "Invalid body."
//                     | _ -> failwith "Invalid pattern.")
//                 patterns

//             failwith "TODO"
//         //Ok(WanderValue.Dataset(results), bindings)
//         | _ -> error "Can only query Datasets." None
//     | Error(errorValue) -> error $"Error handling expression {inputExpression}.\n{errorValue.UserMessage}" None

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
// match valuesToExpressions values [] with
// | Ok(expressions) -> evalExpressions hostFunctions runtimeNetwork expressions
// | _ -> failwith "TODO"
// let mutable result = Ok(Set.empty) //emptyNetwork)
// let mutable cont = true
// let mutable values = values

// while cont && not (List.isEmpty values) do
//     result <- evalValue runtimeNetwork (List.head values)
//     values <- List.tail values

//     match result with
//     | Ok((res)) -> result <- Ok((res))
//     | Error(err) ->
//         result <- Error(err)
//         cont <- false

// result
