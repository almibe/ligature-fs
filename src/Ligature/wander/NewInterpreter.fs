// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.NewInterpreter

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
    | x -> failwith $"TODO - {x}"
// | Expression.AssocArray(values) -> handleAssocArray bindings values
// //    | Expression.Query(expression, conditionals) -> handleQuery bindings expression conditionals
// | Expression.Application(values) -> handleApplication bindings values
// | Expression.Bytes(value) -> Ok(WanderValue.Bytes(value))
// | Expression.Pattern(values) -> handlePattern bindings values
// | Expression.Colon -> failwith "Should never reach"
// | Expression.Slot slot -> Ok(WanderValue.Slot(slot))

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
// let arguments =
//     List.tail values
//     |> List.map (fun expr ->
//         match evalExpression bindings expr with
//         | Ok(res) -> res
//         | Error err -> failwith (err.ToString()))

// match List.tryHead values with
// | Some(Expression.Name(functionName)) ->
//     match readFunction functionName bindings with
//     //        | Some(WanderValue.Array(values)) -> evalArray bindings values arguments
//     //        | Some(WanderValue.Identifier identifer) -> handleIdentifierConcat bindings identifer values.Tail
//     | Some fn ->
//         match fn.Eval arguments bindings with
//         | Ok res -> Ok(res)
//         | Error err -> Error err
//     | None -> error $"Function {functionName} not found." None
// | Some(Expression.Identifier identifier) -> handleIdentifierConcat bindings identifier values.Tail
// | Some(head) -> evalExpression bindings head
// | None -> error "Should never reach, evaling empty Application." None

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

and evalArray bindings array arguments =
    match arguments with
    | [ Expression.Int(index) ] -> Ok(Array.get array (int32 (index)), bindings)
    | _ -> failwith ""

and evalLambda bindings parameters body arguments =
    let mutable i = 0
    let mutable error = None

    let args = failwith "TODO"
    // Array.init (List.length parameters) (fun _ -> WanderValue.Network(Network(Set.empty)))

    List.tryFind
        (fun arg ->
            match evalExpression bindings arg with
            | Ok((res)) ->
                Array.set args i res
                i <- i + 1
                false
            | Error(err) ->
                error <- Some(err)
                true)
        arguments
    |> ignore

    match error with
    | Some(err) -> Error(err)
    | None -> failwith "TODO"
// let mutable scope = addScope bindings
// List.iteri (fun i arg -> scope <- bind (List.item i parameters) arg scope) (Array.toList args)
// evalExpression scope body

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
    match List.length expressions with
    | 0 -> Ok([])
    | 1 -> evalExpression environment (List.head expressions)
    | _ ->
        // let mutable result = Ok(WanderValue.Network(emptyNetwork), bindings)
        // let mutable cont = true
        // let mutable bindings = bindings
        // let mutable expressions = expressions

        // while cont && not (List.isEmpty expressions) do
        //     result <- evalExpression bindings (List.head expressions)
        //     expressions <- List.tail expressions

        //     match result with
        //     | Ok((res, b)) ->
        //         bindings <- b
        //         result <- Ok((res, b))
        //     | Error(err) ->
        //         result <- Error(err)
        //         cont <- false
        failwith "TODO"
//result

and evalValues (environment: Environment) (values: WanderValue list) : Result<WanderValue list, LigatureError> =
    match List.length values with
    | 0 -> Ok([])
    | 1 -> evalValue environment (List.head values)
    | _ ->
        // let mutable result = Ok(WanderValue.Network(emptyNetwork), bindings)
        // let mutable cont = true
        // let mutable bindings = bindings
        // let mutable expressions = expressions

        // while cont && not (List.isEmpty expressions) do
        //     result <- evalExpression bindings (List.head expressions)
        //     expressions <- List.tail expressions

        //     match result with
        //     | Ok((res, b)) ->
        //         bindings <- b
        //         result <- Ok((res, b))
        //     | Error(err) ->
        //         result <- Error(err)
        //         cont <- false
        failwith "TODO"
//result
