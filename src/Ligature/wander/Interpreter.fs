// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Interpreter

open Ligature.Wander.Model
open Ligature.Main
open Ligature.InMemoryNetwork

type Stack = WanderValue list

type Word =
    { Eval: Environment -> Result<Stack, LigatureError> }

and Environment =
    { Words: Map<string, Word>
      Stack: Stack }

let emptyEnvironment =
    { Words = Map.empty
      Stack = List.empty }

let rec evalExpression (environment: Environment) (expression: Expression) : Result<WanderValue list, LigatureError> =
    match expression with
    | Expression.Int value -> Ok((WanderValue.Int value) :: environment.Stack)
    | Expression.String value -> Ok((WanderValue.String value) :: environment.Stack)
    | Expression.Identifier value -> Ok((WanderValue.Identifier value) :: environment.Stack)
    | Expression.Slot value -> Ok((WanderValue.Slot value) :: environment.Stack)
    | Expression.Network(values) -> handlePattern environment values
    | Expression.Word name -> handleWord environment name
    | Expression.Quote quote -> handleQuote environment quote
    | Expression.Colon -> failwith "Not Implemented"
    | Expression.Bytes(_) -> failwith "Not Implemented"
    | Expression.Definition(name, value) -> failwith "Not Implemented"
    | Expression.AssocArray(_) -> failwith "Not Implemented"
// | Expression.AssocArray(values) -> handleAssocArray bindings values
// | Expression.Bytes(value) -> Ok(WanderValue.Bytes(value))

and evalValue (environment: Environment) (value: WanderValue) : Result<WanderValue list, LigatureError> =
    match value with
    | WanderValue.Word word -> handleWord environment word
    | value -> Ok(value :: environment.Stack)

and handleQuote (environment: Environment) (expressions: Expression list) =
    let mutable error = None

    let res: WanderValue list =
        //TODO this doesn't short circuit on first error
        List.map
            (fun expr ->
                match expr with
                | Expression.Int value -> WanderValue.Int value
                | Expression.Word value -> WanderValue.Word value
                //| Expression.Quote quote -> WanderValue.Quote quote)
                | _ -> failwith "TODO")
            expressions

    match error with
    | None -> Ok([ WanderValue.Quote(res) ])
    | Some(err) -> Error(err)

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

and handleEntityDescription bindings (attribute, values) : List<WanderValue> * List<List<WanderValue>> =
    let attribute =
        match evalExpression bindings attribute with
        | Ok(res) -> res
        | _ -> failwith "TODO"

    let values =
        List.map
            (fun value ->
                match evalExpression bindings value with
                | Ok(res) -> res
                | _ -> failwith "TODO")
            values

    (attribute, values)

and handleDatasetRootPattern bindings (entity, entityDescriptions) =
    let mutable triples: Set<Triple> = Set.empty

    let entity =
        match evalExpression bindings entity with
        | Ok([ res ]) -> res
        | _ -> failwith "TODO"

    // let (entityDescriptions: List<WanderValue * List<List<WanderValue>>>) =
    //     List.map (fun entityDescription -> handleEntityDescription bindings entityDescription) entityDescriptions

    List.iter
        (fun entityDescription ->
            let (attribute, values) = entityDescription

            let entity =
                match entity with
                | WanderValue.Slot slot -> PatternIdentifier.Sl slot
                | WanderValue.Identifier identifier -> PatternIdentifier.Id identifier
                | _ -> failwith "TODO"

            let attribute =
                match attribute with
                | WanderValue.Slot slot -> PatternIdentifier.Sl slot
                | WanderValue.Identifier identifier -> PatternIdentifier.Id identifier
                | _ -> failwith "TODO"

            List.iter
                (fun value ->
                    let value =
                        match value with
                        | WanderValue.Int value -> Value.Int value
                        | WanderValue.Bytes value -> Value.Bytes value
                        | WanderValue.Identifier value -> Value.Identifier value
                        | WanderValue.String value -> Value.String value
                        | WanderValue.Slot slot -> Value.Slot(slot)
                        | _ -> failwith "TODO"

                    triples <-
                        Set.add
                            { Entity = entity
                              Attribute = attribute
                              Value = value }
                            triples)
                values)
        //         entityDescriptions
        (failwith "TODO")

    Ok triples

and handlePattern bindings values =
    let res = List.map (fun value -> handleDatasetRootPattern bindings value) values
    let mutable final: Set<Triple> = Set.empty

    List.iter
        (fun ds ->
            match ds with
            | Ok(res: Set<Triple>) -> final <- final + res
            | _ -> failwith "TODO")
        res

    Ok([ WanderValue.Network(InMemoryNetwork(final)) ])

and handleWord environment word =
    match Map.tryFind word environment.Words with
    | Some res -> res.Eval environment
    | None -> error $"Could not find Word {word}" None

and handleIdentifierConcat bindings identifier values = failwith "TODO"
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
    (environment: Environment)
    (expressions: Expression list)
    : Result<WanderValue list, LigatureError> =
        match expressions with
        | [] -> Ok([])
        | [head] -> evalExpression environment head
        | head::tail ->
            match evalExpression environment head with
            | Ok(res) -> evalExpressions { environment with Stack = res } tail
            | Error(err) -> Error(err)

and evalValues (environment: Environment) (values: WanderValue list) : Result<WanderValue list, LigatureError> =
    let mutable result = Ok([])
    let mutable cont = true
    let mutable environment = environment
    let mutable values = values

    while cont && not (List.isEmpty values) do
        result <- evalValue environment (List.head values)
        values <- List.tail values

        match result with
        | Ok((res)) ->
            environment <- { environment with Stack = res }
            result <- Ok((res))
        | Error(err) ->
            result <- Error(err)
            cont <- false

    result
