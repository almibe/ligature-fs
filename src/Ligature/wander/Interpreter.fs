// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Interpreter

open Ligature.Wander.Model
open Ligature.Wander.Bindings
open Ligature.Main
open Ligature.InMemory.Pattern

let readName (name: string) (bindings: Bindings<string, WanderValue>) =
    let namePath = List.ofArray (name.Split("."))
    match read (List.head namePath) bindings with
    | Some(value) ->
        match value with
        | WanderValue.Namespace(values) ->
            match namePath.Tail with
            | [] -> Some(WanderValue.Namespace values)
            | namePath ->
                List.fold
                    (fun values name ->
                        match values with
                        | Some(WanderValue.Namespace(values)) ->
                            match Map.tryFind name values with
                            | Some(res) -> Some(res)
                            | None -> None
                        | None -> failwith "Not Implemented"
                        | Some(value) -> Some value)
                    (Some(WanderValue.Namespace(values)))
                    namePath
        | value -> Some value
    | None -> None

let rec evalExpression bindings expression =
    let rec bindArguments
        (args: Model.Expression list)
        (parameters: string list)
        (bindings: Bindings<_, _>)
        : Result<Bindings<_, _>, LigatureError> =
        if List.length args <> List.length parameters then
            failwith "todo"
        else if List.isEmpty args && List.isEmpty parameters then
            Ok bindings
        else
            let arg = List.head args
            let parameter = List.head parameters
            let value = evalExpression bindings arg

            match value with
            | Ok(value, _) -> Ok(bind parameter value bindings)
            | Error(err) -> Error(err)

    match expression with
    | Expression.Name(name) ->
        match readName name bindings with
        | Some(value) -> Ok((value, bindings))
        | None -> error $"Could not read {name}" None
    | Expression.Grouping(expressions) ->
        let bindings' = addScope bindings

        match evalExpressions bindings' expressions with
        | Error(err) -> Error(err)
        | Ok((res, _)) -> Ok((res, bindings))
    | Expression.Let(name, expression) ->
        let res = evalExpression bindings expression

        match res with
        | Ok((value, _)) ->
            let bindings = bind name value bindings
            Ok((value, bindings))
        | Error(_) -> res
    | Expression.FunctionCall(name, args) -> failwith "TODO"
    // let args = List.map ( fun a ->
    //     match evalExpression bindings a with
    //     | Ok(v, _) -> failwith "todo" //Expression.Value(v)
    //     | Error(err) -> todo)
    //             args
    //     match Bindings.read name bindings with
    //     | Some(WanderValue.HostFunction(funct)) ->
    //         match funct.Run args bindings with
    //         | Ok res -> Ok (res, bindings)
    //         | Error(err) -> Error(err)
    //     // | Some(WanderValue.Lambda(parameters, body)) ->
    //     //     match bindArguments args parameters bindings with
    //     //     | Ok(bindings) -> evalExpression bindings body
    //     //     | Error(err) -> Error(err)
    //     | None -> error $"{name} function not found." None
    //     | _ -> todo //type error
    // // | Expression.Conditional(conditional) ->
    // //     let ifCondition = evalExpression bindings conditional.ifCase.condition
    // //     let mutable result = None

    //     result <-
    //         match ifCondition with
    //         | Ok(WanderValue.Bool(true), bindings) -> Some (evalExpression bindings conditional.ifCase.body)
    //         | Ok(WanderValue.Bool(false), _) -> None
    //         | Ok _ -> Some(error "Type mismatch, expecting boolean." None)
    //         | Error x -> Some(Error x)

    // let mutable elsifCases = conditional.elsifCases
    // while Option.isNone result && not elsifCases.IsEmpty do
    //     let case = elsifCases.Head
    //     result <-
    //         match evalExpression bindings case.condition with
    //         | Ok(WanderValue.Bool(true), bindings) -> Some (evalExpression bindings case.body)
    //         | Ok(WanderValue.Bool(false), _) -> None
    //         | Ok _ -> Some(error "Type mismatch, expecting boolean." None)
    //         | Error x -> Some(Error x)
    //     elsifCases <- elsifCases.Tail

    // match result with
    // | None -> evalExpression bindings conditional.elseBody
    // | Some(result) -> result
    | Expression.Int value -> Ok(WanderValue.Int value, bindings)
    | Expression.String value -> Ok(WanderValue.String value, bindings)
    | Expression.Bool value -> Ok(WanderValue.Bool value, bindings)
    | Expression.Identifier id -> Ok(WanderValue.Identifier id, bindings)
    | Expression.Array(expressions) ->
        let mutable error = None

        let res: WanderValue array =
            //TODO this doesn't short circuit on first error
            Array.map
                (fun e ->
                    match evalExpression bindings e with
                    | Ok(value, _) -> value
                    | Error(err) ->
                        if Option.isNone error then
                            error <- Some(err)

                        //WanderValue.Dataset(new InMemoryNetwork(Set.empty)))
                        failwith "TODO")
                (Array.ofList expressions)

        match error with
        | None -> Ok((WanderValue.Array(res), bindings))
        | Some(err) -> Error(err)
    | Expression.Lambda(parameters, body) -> handleLambda bindings parameters body
    | Expression.Record(values) -> handleRecord bindings values
    //    | Expression.Query(expression, conditionals) -> handleQuery bindings expression conditionals
    | Expression.Application(values) -> handleApplication bindings values
    | Expression.Bytes(value) -> Ok(WanderValue.Bytes(value), bindings)
    | Expression.Pattern(values) -> handlePattern bindings values
    | Expression.Colon -> failwith "Should never reach"
    | Expression.Slot slot -> Ok(WanderValue.Slot(slot), bindings)

and callFunction (fn: Function) (args: WanderValue list) (bindings: Bindings<_, _>) =
    match fn with
    | Function.Lambda(parameters, body) ->
        if (List.length parameters) = (List.length args) then
            let binding =
                (List.zip parameters args)
                |> List.fold (fun bindings (name, value) -> bind name value bindings) bindings

            evalExpression binding body |> Result.map (fun (value, _) -> value)
        else
            error $"Improper parameters passed to lambda - {parameters}" None
    | Function.HostFunction(hf) -> hf.Run args bindings

and handleRecord bindings values =
    let res =
        List.map
            (fun (name, expr) ->
                match evalExpression bindings expr with
                | Error(err) -> failwith "TODO"
                | Ok((res, _)) -> (name, res))
            values

    let v = List.fold (fun state (name, value) -> Map.add name value state) (Map []) res
    Ok(WanderValue.Namespace(v), bindings)

and handleEntityDescription bindings (attribute, values) =
    let attribute =
        match evalExpression bindings attribute with
        | Ok(res, _) -> res
        | _ -> failwith "TODO"

    let values =
        List.map
            (fun value ->
                match evalExpression bindings value with
                | Ok(res, _) -> res
                | _ -> failwith "TODO")
            values

    (attribute, values)

and handleDatasetRootPattern bindings (entity, entityDescriptions) =
    let mutable statements: Set<PatternStatement> = Set.empty

    let entity =
        match evalExpression bindings entity with
        | Ok(res, _) -> res
        | _ -> failwith "TODO"

    let entityDescriptions =
        List.map (fun entityDescription -> handleEntityDescription bindings entityDescription) entityDescriptions

    List.iter
        (fun entityDescription ->
            let (attribute, values) = entityDescription

            let entity =
                match entity with
                | WanderValue.Slot slot -> PatternIdentifier.Slot slot
                | WanderValue.Identifier identifier -> PatternIdentifier.Identifier identifier
                | _ -> failwith "TODO"

            let attribute =
                match attribute with
                | WanderValue.Slot slot -> PatternIdentifier.Slot slot
                | WanderValue.Identifier identifier -> PatternIdentifier.Identifier identifier
                | _ -> failwith "TODO"

            List.iter
                (fun value ->
                    let value =
                        match value with
                        | WanderValue.Int value -> PatternValue.Value(Value.Int value)
                        | WanderValue.Bytes value -> PatternValue.Value(Value.Bytes value)
                        | WanderValue.Identifier value -> PatternValue.Value(Value.Identifier value)
                        | WanderValue.String value -> PatternValue.Value(Value.String value)
                        | WanderValue.Slot slot -> PatternValue.Slot(slot)
                        | _ -> failwith "TODO"

                    statements <-
                        Set.add
                            { Entity = entity
                              Attribute = attribute
                              Value = value }
                            statements)
                values)
        entityDescriptions

    Ok statements

and handlePattern bindings values =
    let res = List.map (fun value -> handleDatasetRootPattern bindings value) values
    let mutable final: Set<PatternStatement> = Set.empty

    List.iter
        (fun ds ->
            match ds with
            | Ok(res: Set<PatternStatement>) -> final <- final + res
            | _ -> failwith "TODO")
        res

    Ok(WanderValue.Pattern(InMemoryPattern(final)), bindings)

and handleApplication bindings values =
    let arguments = List.tail values

    match List.tryHead values with
    | Some(Expression.Name(functionName)) ->
        match readName functionName bindings with
        | Some(WanderValue.Function(Function.Lambda(parameters, body))) -> evalLambda bindings parameters body arguments
        | Some(WanderValue.Function(Function.HostFunction(hostFunction))) ->
            evalHostFunction bindings hostFunction arguments
        | Some(WanderValue.Array(values)) -> evalArray bindings values arguments
        | Some(WanderValue.Identifier identifer) -> handleIdentifierConcat bindings identifer values.Tail
        | Some _ -> error "Improper application." None
        | None -> error $"Function {functionName} not found." None
    | Some(Expression.Identifier identifier) -> handleIdentifierConcat bindings identifier values.Tail
    | Some(head) -> evalExpression bindings head
    | None -> error "Should never reach, evaling empty Application." None

and handleIdentifierConcat bindings identifier values =
    List.mapi
        (fun i value ->
            if i % 2 = 1 then
                match evalExpression bindings value with
                | Ok(WanderValue.Identifier identifier, _) -> Some(readIdentifier identifier)
                | Ok(WanderValue.String string, _) -> Some(string)
                | Ok(WanderValue.Int int, _) -> Some(int.ToString())
                | value -> failwith $"Unexpected value: {value}"
            else if value = Expression.Colon then
                None
            else
                failwith "error")
        values
    |> List.fold
        (fun state current ->
            match current with
            | Some value -> state + value
            | None -> state)
        (readIdentifier identifier)
    |> (fun res ->
        match Ligature.Main.identifier res with
        | Ok identifier -> Ok(WanderValue.Identifier(identifier), bindings)
        | Error err -> failwith "todo")

and evalHostFunction bindings hostFunction arguments =
    let values =
        List.map
            (fun arg ->
                match evalExpression bindings arg with
                | Ok((value, _)) -> value
                | Error(err) -> failwith $"Error calling Host Function: {err}")
            arguments

    match hostFunction.Run values bindings with
    | Ok(res) -> Ok(res, bindings)
    | Error(err) -> Error(err)

and evalArray bindings array arguments =
    match arguments with
    | [ Expression.Int(index) ] -> Ok(Array.get array (int32 (index)), bindings)
    | _ -> failwith ""

and evalLambda bindings parameters body arguments =
    let mutable i = 0
    let mutable error = None

    let args =
        Array.init (List.length parameters) (fun _ -> WanderValue.Pattern(InMemoryPattern(Set.empty)))

    List.tryFind
        (fun arg ->
            match evalExpression bindings arg with
            | Ok((res, _)) ->
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
    | None ->
        let mutable scope = Bindings.addScope bindings
        List.iteri (fun i arg -> scope <- Bindings.bind (List.item i parameters) arg scope) (Array.toList args)
        evalExpression scope body

and handleLambda bindings parameters body =
    Ok(WanderValue.Function(Function.Lambda(parameters, body)), bindings)

// and checkPattern bindings (input: INetwork) (pattern: DatasetPatternRoot list) : bool =
//     //NOTE: calling evalExpression below is wrong since it will eval any names used for pattern matching
//     //this only works for matching with literals and no destructuring
//     match evalExpression bindings (Expression.Pattern(pattern)) with
//     | Ok(WanderValue.Pattern(pattern), _) -> failwith "TODO"
//     // match (pattern.AllStatements (), input.AllStatements ()) with
//     // | (Ok(pattern), Ok(statements)) -> Set.isSubset (Set.ofList pattern) (Set.ofList statements)
//     // | _ -> failwith "Error"
//     | _ -> failwith "Error"

// and handleQuery bindings inputExpression patterns =
//     let mutable results = emptyInMemoryNetwork

//     match evalExpression bindings inputExpression with
//     | Ok(input, _) ->
//         match input with
//         | WanderValue.Pattern(input) ->
//             List.iter
//                 (fun (pattern, body) ->
//                     match pattern with
//                     | Expression.Pattern(pattern) ->
//                         if checkPattern bindings input pattern then
//                             match evalExpression bindings body with
//                             | Ok(WanderValue.Dataset(dataset), _) ->
//                                 failwith "TODO"
//                                 // match dataset.AllStatements () with
//                                 // | Ok(statements) -> results <- InMemoryNetwork (results.statements + (Set.ofList statements))
//                                 // | _ -> failwith "Error reading Statements"
//                             | _ -> failwith "Invalid body."
//                     | _ -> failwith "Invalid pattern.")
//                 patterns

//             failwith "TODO"
//         //Ok(WanderValue.Dataset(results), bindings)
//         | _ -> error "Can only query Datasets." None
//     | Error(errorValue) -> error $"Error handling expression {inputExpression}.\n{errorValue.UserMessage}" None

and evalExpressions
    (bindings: Bindings.Bindings<_, _>)
    (expressions: Expression list)
    : Result<(WanderValue * Bindings.Bindings<_, _>), LigatureError> =
    match List.length expressions with
    | 0 -> Ok(WanderValue.Pattern(emptyPattern), bindings)
    | 1 -> evalExpression bindings (List.head expressions)
    | _ ->
        let mutable result = Ok(WanderValue.Pattern(emptyPattern), bindings)
        let mutable cont = true
        let mutable bindings = bindings
        let mutable expressions = expressions

        while cont && not (List.isEmpty expressions) do
            result <- evalExpression bindings (List.head expressions)
            expressions <- List.tail expressions

            match result with
            | Ok((res, b)) ->
                bindings <- b
                result <- Ok((res, b))
            | Error(err) ->
                result <- Error(err)
                cont <- false

        result
