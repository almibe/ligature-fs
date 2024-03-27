// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Bend.Lib.Ligature
open Ligature
open Ligature.Bend.Model

let datasetsFun (ligature: ILigature) = BendValue.HostFunction (
    new HostFunction((fun _ _ ->
        match ligature.AllDatasets () with
        | Ok(datasets) ->
            datasets
            |> List.map (fun (Dataset name) -> BendValue.String name)
            |> BendValue.Array
            |> Ok
        | Error(err) -> failwith "todo")))

let createDatasetFun (ligature: ILigature) = BendValue.HostFunction (
    new HostFunction((fun args _ ->
        match args with
        | [BendValue.String(datasetName)] ->
            match ligature.CreateDataset (Dataset datasetName) with
            | Ok(_) -> Ok(BendValue.Nothing)
            | Error(errorValue) -> failwith "Not Implemented"
        | _ -> failwith "TODO")))

let removeDatasetFun (ligature: ILigature) = BendValue.HostFunction (
    new HostFunction((fun args _ ->
        match args with
        | [BendValue.String(datasetName)] ->
            match ligature.RemoveDataset (Dataset datasetName) with
            | Ok(_) -> Ok(BendValue.Nothing)
            | Error(err) -> failwith "todo"
        | _ -> failwith "")))

let datasetExists (instance: ILigature) = BendValue.HostFunction (
    new HostFunction(fun args _ ->
        match args.Head with
        | BendValue.String(name) -> 
            let dataset = Dataset(name)
            match instance.DatasetExists dataset with
            | Ok(result) -> Ok(BendValue.Bool(result))
            | Error(err) -> Error(err)
        | _ -> error "Could not check for Dataset" None))

let valueToWanderValue (value: Value): BendValue =
    match value with
    | Value.Identifier i -> BendValue.Identifier i
    | Value.Integer i -> BendValue.Int i
    | Value.String s -> BendValue.String s

let rec statementsToList (statements: Statement list) (results: BendValue list): BendValue list =
    if List.isEmpty statements then
        results
    else
        let statement = statements.Head
        let entity = BendValue.Identifier(statement.Entity)
        let attribute = BendValue.Identifier(statement.Attribute)
        let value = valueToWanderValue statement.Value
        statementsToList (statements.Tail) (List.append results [BendValue.Array[entity; attribute; value]])

let allStatementsFun (instance: ILigature) = BendValue.HostFunction (
    new HostFunction(fun args _ ->
        let datasetName = args.Head
        match datasetName with
        | BendValue.String(name) ->
            let dataset = Dataset name
            instance.Query dataset (fun tx ->
                match tx.AllStatements () with
                | Ok(statements) ->
                    Ok(BendValue.Array(statementsToList statements []))
                | Error(err) -> Error(err)
            )
        | _ -> error "Improper arguments could not run allStatements." None
    ))

let matchStatements (query: IQueryTx) = BendValue.HostFunction (
    new HostFunction(fun args bindings ->
        error "todo - inside match" None
    )
)

let queryFun (instance: ILigature) = BendValue.HostFunction (
    new HostFunction(fun args bindings ->
        match args with
        | [BendValue.String(datasetName); BendValue.Lambda(_parameters, body)] ->
            let dataset = Dataset(datasetName)
            let res = instance.Query dataset (fun tx ->
                let bindings' = Bend.Bindings.bind "match" (matchStatements tx) bindings
                error "todo - inside query" None)
            res
        | _ -> error "Improper arguments could not run query." None
    ))

/// A NativeFunction that does a single match against a given Dataset.
/// Internally it starts a query transaction and then runs a single function in the tx.
let matchCommand (instance: ILigature) = BendValue.HostFunction (
    new HostFunction(fun args bindings ->
        match args with
        | [ BendValue.String(datasetName); 
            entity;
            attribute;
            value] ->
                let dataset = Dataset(datasetName)
                let entity =
                    match entity with
                    | BendValue.Identifier(i) -> Ok (Some i)
                    | BendValue.Nothing -> Ok None
                    | _ -> error "Invalid Entity passed to match." None
                let attribute =
                    match attribute with
                    | BendValue.Identifier(i) -> Ok (Some i)
                    | BendValue.Nothing -> Ok None
                    | _ -> error "Invalid Attribute passed to match." None
                let value =
                    match value with
                    | BendValue.Identifier(i) -> Ok (Some (Value.Identifier i))
                    | BendValue.Nothing -> Ok None
                    | _ -> failwith "TODO"
                match (entity, attribute, value) with
                | (Ok(entity), Ok(attribute), Ok(value)) ->
                    instance.Query dataset (fun tx ->
                        match tx.MatchStatements entity attribute value with
                        | Ok(results) -> failwith "TODO" //Ok(WanderValue.Tuple(statementsToTuple results []))
                        | Error(err) -> Error(err)
                    )
                | _ -> error "Could not call match." None //TODO should return actual error
        | _ -> error "Improper arguments passed to match." None
    )
)

/// A Native Function that write Statements to a Dataset.
/// Example: addStatements("dataset" (<a> <b> <c>)(<a> <b> "Test") (<a> <b> 432))
let addStatementsFun (instance: ILigature) = BendValue.HostFunction (
    new HostFunction(fun args _ ->
        match args with
        | [(BendValue.String(name)); BendValue.Array(statements)] ->
            let dataset = Dataset(name)
            let writeRes = instance.Write dataset (fun tx ->
                let rec addStatements statements =
                    if not (List.isEmpty statements) then
                        let statement = statements.Head
                        match statement with
                        | BendValue.Statement(statement) -> 
                            match tx.AddStatement statement with
                            | Ok _ -> addStatements statements.Tail
                            | Error err -> Error err
                        | _ -> error "Add Statements only accepts Statements." None
                    else
                        Ok ()
                addStatements statements)
            match writeRes with
            | Ok _ -> Ok BendValue.Nothing
            | Error err -> Error err
        | _ -> error "Improper call to addStatements." None
    ))

/// A Native Function that write Statements to a Dataset.
/// Example: addStatements("dataset" (<a> <b> <c>)(<a> <b> "Test") (<a> <b> 432))
let removeStatementsFun (instance: ILigature) = BendValue.HostFunction (
    new HostFunction(fun args _ ->
        let datasetName = args.Head
        let statements = args.Tail.Head
        match (datasetName, statements) with
        | (BendValue.String(name)), BendValue.Array(statements) ->
            let dataset = Dataset(name)
            let writeRes = instance.Write dataset (fun tx ->
                let rec addStatement statements =
                    if not (List.isEmpty statements) then
                        let statement = statements.Head
                        // match statements.Head with
                        // | Tuple(statement) ->
                        match statement with
                        | BendValue.Array(statement) ->
                            let entity = statement.Head
                            let attribute = statement.Tail.Head
                            let value = statement.Tail.Tail.Head
                            match (entity, attribute, value) with
                            | (BendValue.Identifier(entity), BendValue.Identifier(attribute), BendValue.Identifier(value)) ->
                                match tx.AddStatement (Ligature.statement entity attribute (Value.Identifier(value))) with
                                | Ok _ -> addStatement statements.Tail
                                | Error err -> Error err
                            | _ -> error "Invalid Statement contents." None
                        | _ -> error "Error Statements must be expressed as Tuples." None
                    else
                        Ok ()
                addStatement statements)
            match writeRes with
            | Ok _ -> Ok BendValue.Nothing
            | Error err -> Error err
        | _ -> error "Improper call to removeStatements." None
    ))

let ligatureLib (ligature: ILigature) = BendValue.Record (Map [
    ("datasets", datasetsFun ligature)
    ("createDataset", createDatasetFun ligature)
    ("removeDataset", removeDatasetFun ligature)
    ("allStatements", allStatementsFun ligature)
    ("query", queryFun ligature)
    ("addStatements", addStatementsFun ligature)
    ("removeStatements", removeStatementsFun ligature)
])
