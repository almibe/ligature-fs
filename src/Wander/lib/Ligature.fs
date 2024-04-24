// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Ligature

open Ligature
open Ligature.Wander.Model
open System
open Ligature.Wander.InMemoryDataset

let emptySet = WanderValue.Dataset(InMemoryDataset(Set.empty))

let datasetsFun (ligature: ILigature) =
    WanderValue.Function(
        Function.HostFunction(
            HostFunction(
                (fun _ _ ->
                    match ligature.AllDatasets() with
                    | Ok(datasets) ->
                        datasets
                        |> Seq.map (fun (DatasetName name) -> WanderValue.String name)
                        |> Array.ofSeq
                        |> WanderValue.Array
                        |> Ok
                    | Error(err) -> failwith "todo")
            )
        )
    )

let createDatasetFun (ligature: ILigature) =
    WanderValue.Function(
        Function.HostFunction(
            HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.String(datasetName) ] ->
                        match ligature.CreateDataset(DatasetName datasetName) with
                        | Ok(_) -> Ok(emptySet)
                        | Error(errorValue) -> failwith "Not Implemented"
                    | _ -> failwith "TODO")
            )
        )
    )

let removeDatasetFun (ligature: ILigature) =
    WanderValue.Function(
        Function.HostFunction(
            HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.String(datasetName) ] ->
                        match ligature.RemoveDataset(DatasetName datasetName) with
                        | Ok(_) -> Ok(emptySet)
                        | Error(err) -> failwith "todo"
                    | _ -> failwith "")
            )
        )
    )

let datasetExists (instance: ILigature) =
    WanderValue.Function(
        Function.HostFunction(
            HostFunction(fun args _ ->
                match args.Head with
                | WanderValue.String(name) ->
                    let dataset = DatasetName(name)

                    match instance.DatasetExists dataset with
                    | Ok(result) -> Ok(WanderValue.Bool(result))
                    | Error(err) -> Error(err)
                | _ -> error "Could not check for Dataset" None)
        )
    )

let valueToWanderValue (value: Value) : WanderValue =
    match value with
    | Value.Identifier i -> WanderValue.Identifier i
    | Value.Int i -> WanderValue.Int i
    | Value.String s -> WanderValue.String s
    | Value.Bytes b -> WanderValue.Bytes b

let allStatementsFun (instance: ILigature) =
    WanderValue.Function(
        Function.HostFunction(
            HostFunction(fun args _ ->
                match args with
                | [ WanderValue.String(name) ] ->
                    let dataset = DatasetName name

                    match instance.AllStatements dataset with
                    | Ok(statements) ->
                        statements
                        |> Seq.map (fun statement -> WanderValue.Statement(statement))
                        |> fun statements -> Ok(WanderValue.Array((Array.ofSeq statements)))
                    | Error(err) -> Error(err)
                | _ -> error "Illegal call to allStatements." None)
        )
    )

let readFun (instance: ILigature) =
    WanderValue.Function(
        Function.HostFunction(
            HostFunction(fun args _ ->
                match args with
                | [ WanderValue.String(name) ] ->
                    let dataset = DatasetName name

                    match instance.AllStatements dataset with
                    | Ok(statements) ->
                        statements
                        |> Seq.map (fun statement -> WanderValue.Statement(statement))
                        |> fun statements -> Ok(WanderValue.Array((Array.ofSeq statements)))
                    | Error(err) -> Error(err)
                | _ -> error "Illegal call to allStatements." None)
        )
    )

let matchStatements (query: IDataset) =
    WanderValue.Function(Function.HostFunction(new HostFunction(fun args bindings -> error "todo - inside match" None)))

let queryFun (instance: ILigature) =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(fun args bindings ->
                match args with
                | [ WanderValue.String(datasetName); WanderValue.Function(Function.Lambda(_parameters, body)) ] ->
                    let dataset = DatasetName(datasetName)

                    let res =
                        instance.Query dataset (fun tx ->
                            //let bindings' = Wander.Bindings.bind "match" (matchStatements tx) bindings
                            //error "todo - inside query" None
                            //Ok(WanderValue.Nothing))
                            failwith "TODO")

                    //res
                    failwith "TODO"
                | _ -> error "Improper arguments could not run query." None)
        )
    )

/// A NativeFunction that does a single match against a given Dataset.
/// Internally it starts a query transaction and then runs a single function in the tx.
let matchFun (instance: ILigature) =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(fun args bindings ->
                match args with
                | [ WanderValue.String(datasetName); entity; attribute; value ] ->
                    let dataset = DatasetName datasetName

                    let entity =
                        match entity with
                        | WanderValue.Identifier(i) -> Ok(Some i)
                        | WanderValue.Dataset(_) -> Ok None
                        | _ -> error "Invalid Entity passed to match." None

                    let attribute =
                        match attribute with
                        | WanderValue.Identifier(i) -> Ok(Some i)
                        | WanderValue.Dataset(_) -> Ok None
                        | _ -> error "Invalid Attribute passed to match." None

                    let value =
                        match value with
                        | WanderValue.Identifier(i) -> Ok(Some(Value.Identifier i))
                        | WanderValue.Dataset(_) -> Ok None
                        | WanderValue.Int(value) -> Ok(Some(Value.Int value))
                        | WanderValue.String(value) -> Ok(Some(Value.String value))
                        | _ -> error "Invalid Value passed to match." None

                    match (entity, attribute, value) with
                    | (Ok(entity), Ok(attribute), Ok(value)) ->
                        let res =
                            failwith "TODO"
                            // instance.Query dataset (fun tx -> tx.MatchStatements entity attribute value)

                        match res with
                        | Ok res -> Ok(WanderValue.Dataset res)
                        | Error err -> Error err
                    | _ -> error "Could not call match." None //TODO should return actual error
                | _ -> error "Improper arguments passed to match." None)
        )
    )

/// A Host Function that writes Statements to a Dataset.
/// Example: Ligature.addStatements "dataset" [<a> <b> <c>, <a> <b> "Test", <a> <b> 432]
let addStatementsFun (instance: ILigature) =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(fun args _ ->
                match args with
                | [ (WanderValue.String(name)); WanderValue.Array(statements) ] ->
                    let dataset = DatasetName name

                    let statements =
                        List.map
                            (fun value ->
                                match value with
                                | WanderValue.Statement(statement) -> statement
                                | _ -> failwith "todo")
                            (List.ofArray statements)

                    match instance.AddStatements dataset statements with
                    | Ok _ -> Ok(emptySet)
                    | Error err -> Error err
                | [ (WanderValue.String(name)); WanderValue.Dataset(statements) ] ->
                    let dataset = DatasetName name

                    match statements.AllStatements() with
                    | Ok statements ->
                        match instance.AddStatements dataset statements with
                        | Ok _ -> Ok(emptySet)
                        | Error err -> Error err
                    | Error err -> Error err
                | _ -> error "Improper call to addStatements." None)
        )
    )

/// A Host Function that removes Statements from a Dataset.
/// Example: Ligature.removeStatements "dataset" [<a> <b> <c>, <a> <b> "Test", <a> <b> 432]
let removeStatementsFun (instance: ILigature) =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(fun args _ ->
                match args with
                | [ WanderValue.String(name); WanderValue.Array(statements) ] ->
                    let dataset = DatasetName name

                    let statements =
                        List.map
                            (fun value ->
                                match value with
                                | WanderValue.Statement(statement) -> statement
                                | _ -> failwith "todo")
                            (List.ofArray statements)

                    match instance.RemoveStatements dataset statements with
                    | Ok _ -> Ok(emptySet)
                    | Error err -> Error err
                | [ WanderValue.String(name); WanderValue.Dataset(statements) ] ->
                    let dataset = DatasetName name

                    match statements.AllStatements() with
                    | Ok statements ->
                        match instance.RemoveStatements dataset statements with
                        | Ok _ -> Ok(emptySet)
                        | Error err -> Error err
                    | Error err -> Error err
                | _ -> error "Improper call to removeStatements." None)
        )
    )

let newIdFun (instance: ILigature) =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(fun args _ ->
                match args with
                | [ WanderValue.String(prefix) ] ->
                    let timeStamp = DateTime.Now.Ticks

                    match identifier (prefix + timeStamp.ToString()) with
                    | Ok(id) -> Ok(WanderValue.Identifier(id))
                    | Error(err) -> Error(err)
                | _ -> error "Improper call to removeStatements." None)
        )
    )

let ligatureLib (ligature: ILigature) =
    WanderValue.Record(
        Map
            [ ("datasets", datasetsFun ligature)
              ("createDataset", createDatasetFun ligature)
              ("removeDataset", removeDatasetFun ligature)
              ("allStatements", allStatementsFun ligature)
              ("query", queryFun ligature)
              ("match", matchFun ligature)
              ("addStatements", addStatementsFun ligature)
              ("removeStatements", removeStatementsFun ligature)
              ("newId", newIdFun ligature) ]
    )
