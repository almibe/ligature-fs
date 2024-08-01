// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Interpreter

open Ligature.Wander.Model
open Ligature.Main
open Ligature.InMemoryNetwork

let rec evalExpression
    (hostFunctions: Map<string, HostFunction>)
    (runtimeNetwork: Network)
    (expression: Expression)
    : Result<Network, LigatureError> =
    match expression with
    | Expression.Network(network) -> Ok(Set.union runtimeNetwork network) //runtimeNetwork.Union(network)) //Ok(WanderValue.Network(network) :: stack)
    | Expression.Call(name, args) -> handleWord hostFunctions runtimeNetwork name args

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

and handleAssocArray bindings values = failwith "TODO"
// let res =
//     List.map
//         (fun (name, expr) ->
//             match evalExpression bindings expr with
//             | Error(err) -> failwith "TODO"
//             | Ok((res)) -> (name, res))
//         values

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

and handleWord (hostFunctions: Map<string, HostFunction>) network word args =
    let res =
        Set.filter
            (fun (e, a, v) ->
                match (e, a, v) with
                | (PatternWord.Word(entity), PatternWord.Word(attribute), LigatureValue.Quote(_)) ->
                    entity = word && attribute = Word("=")
                | (PatternWord.Word(entity), PatternWord.Word(attribute), LigatureValue.HostFunction(name)) ->
                    entity = word && attribute = Word("=")
                | _ -> false)
            network
    // network.Query
    //     (networkOf ([ (PatternWord.Word(Word(word)), PatternWord.Word(Word("=")), Value.Slot(Slot(Some("name")))) ]))
    //     (networkOf ([ (PatternWord.Word(Word(word)), PatternWord.Word(Word("=")), Value.Slot(Slot(Some("name")))) ]))

    match List.ofSeq (res) with
    | [] -> error $"Could not find Word, {word}" None
    | [ (_, _, LigatureValue.Quote(names, quote)) ] -> evalQuote hostFunctions network names quote
    //    | [(_, _, LigatureValue.HostFunction(name))] -> evalHostFunction hostFunctions network name
    | _ -> error $"Multiple matches found for Word, {word}" None

and handleIdentifierConcat words stack identifier values = failwith "TODO"
// List.mapi
//     (fun i value ->
//         if i % 2 = 1 then
//             match evalExpression bindings value with
//             | Ok(WanderValue.Identifier identifier) -> Some(readIdentifier identifier)
//             | Ok(WanderValue.String string) -> Some(string)
//             | Ok(WanderValue.Int int) -> Some(int.ToString())
//             | value -> failwith $"Unexpected value: {value}"
//         else if value = Expression.Colon then
//             None
//         else
//             failwith "error")
//     values
// |> List.fold
//     (fun state current ->
//         match current with
//         | Some value -> state + value
//         | None -> state)
//     (readIdentifier identifier)
// |> (fun res ->
//     match Ligature.Main.identifier res with
//     | Ok identifier -> Ok(WanderValue.Identifier(identifier))
//     | Error err -> failwith "todo")

// and evalHostFunction bindings hostFunction arguments =
//     let values =
//         List.map
//             (fun arg ->
//                 match evalExpression bindings arg with
//                 | Ok((value, _)) -> value
//                 | Error(err) -> failwith $"Error calling Host Function: {err}")
//             arguments

//     match hostFunction.Run values bindings with
//     | Ok(res) -> Ok(res, bindings)
//     | Error(err) -> Error(err)

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
    (hostFunctions: Map<string, HostFunction>)
    (runtimeNetwork: Network)
    (expressions: Expression list)
    : Result<Network, LigatureError> =
    match expressions with
    | [] -> Ok(runtimeNetwork)
    | [ head ] -> evalExpression hostFunctions runtimeNetwork head
    | head :: tail ->
        match evalExpression hostFunctions runtimeNetwork head with
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
            | LigatureValue.Quote(p, q) :: tail -> failwith "TODO"
            | _ ->
                valuesToExpressions
                    []
                    (List.append expressions [ Expression.Call(w, { parameterNames = []; quote = [] }) ])
        | _ -> error "Invalid Quote" None

and evalQuote
    (hostFunctions)
    (runtimeNetwork: Network)
    (names: string list)
    (values: LigatureValue list)
    : Result<Network, LigatureError> =
    match valuesToExpressions values [] with
    | Ok(expressions) -> evalExpressions hostFunctions runtimeNetwork expressions
    | _ -> failwith "TODO"
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
