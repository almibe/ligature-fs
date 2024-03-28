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

let allStatementsFun (instance: ILigature) = BendValue.HostFunction (
    new HostFunction(fun args _ ->
        match args with
        | [BendValue.String(name)] ->
            let dataset = Dataset name
            match instance.AllStatements dataset with
            | Ok(statements) ->
                statements
                |> List.map (fun statement -> BendValue.Statement(statement))
                |> fun statements -> Ok(BendValue.Array(statements))
            | Error(err) -> Error(err)
        | _ -> failwith "todo"
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

/// A Host Function that writes Statements to a Dataset.
/// Example: Ligature.addStatements "dataset" [<a> <b> <c>, <a> <b> "Test", <a> <b> 432]
let addStatementsFun (instance: ILigature) = BendValue.HostFunction (
    new HostFunction(fun args _ ->
        match args with
        | [(BendValue.String(name)); BendValue.Array(statements)] ->
            let dataset = Dataset(name)
            let statements = List.map (fun value -> 
                match value with
                | BendValue.Statement(statement) -> statement
                | _ -> failwith "todo") statements
            match instance.AddStatements dataset statements with
            | Ok _ -> Ok BendValue.Nothing
            | Error err -> Error err
        | _ -> error "Improper call to addStatements." None
    ))

/// A Host Function that removes Statements from a Dataset.
/// Example: Ligature.removeStatements "dataset" [<a> <b> <c>, <a> <b> "Test", <a> <b> 432]
let removeStatementsFun (instance: ILigature) = BendValue.HostFunction (
    new HostFunction(fun args _ ->
        match args with
        | [BendValue.String(name); BendValue.Array(statements)] ->
            printf "%A %A" name statements
            let dataset = Dataset(name)
            let statements = List.map (fun value -> 
                match value with
                | BendValue.Statement(statement) -> statement
                | _ -> failwith "todo") statements
            match instance.RemoveStatements dataset statements with
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
