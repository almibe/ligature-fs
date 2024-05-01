// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Dataset

open Ligature.Wander.Model
open Ligature

let patternStatementToStatement (pattern: PatternStatement) : Statement option =
    match pattern with
    | { Entity = PatternIdentifier.Identifier(entity)
        Attribute = PatternIdentifier.Identifier(attribute) } -> failwith "TODO"
    | _ -> failwith "TODO"

let extract (dataset: IDataset) (data: IPattern) =
    dataset.Extract data
    |> List.map (fun res ->
        res
        |> Map.toSeq
        |> Seq.map (fun (k, v) ->
            (k.Name,
                match v with
                | Value.Int value -> WanderValue.Int value
                | Value.Bytes value -> WanderValue.Bytes value
                | Value.Identifier value -> WanderValue.Identifier value
                | Value.String value -> WanderValue.String value))
        |> Map.ofSeq
        |> WanderValue.Record)
    |> Array.ofList
    |> WanderValue.Array
    |> Ok

let patternToDataset (pattern: IPattern): IDataset =
    match pattern.Dataset with
    | Some dataset -> dataset
    | _ -> failwith "TODO"

let extractFunction<'t> =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(fun args _ ->
                match args with
                | [ WanderValue.Dataset(dataset); WanderValue.Pattern(data) ] -> extract dataset data
                | [ WanderValue.Dataset(dataset); WanderValue.Dataset(data) ] -> failwith "TODO"
                | [ WanderValue.Pattern(dataset); WanderValue.Pattern(data) ] -> extract (patternToDataset dataset) data
                | [ WanderValue.Pattern(dataset); WanderValue.Dataset(data) ] -> failwith "TODO"
                | value -> error $"Unexpected value passed to Dataset.extract - {value}." None)
        )
    )

let countFunction<'t> =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(fun args _ ->
                match args with
                | [ WanderValue.Pattern(pattern) ] -> Ok(WanderValue.Int(Set.count pattern.PatternStatements))
                | value -> error $"Unexpected value - {value}." None)
        )
    )

let extractsFunction<'t> =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(fun args _ ->
                match args with
                | [ WanderValue.Pattern(statements) ] ->
                    let result =
                        Set.exists
                            (fun (statement: PatternStatement) ->
                                let entity =
                                    match statement.Entity with
                                    | PatternIdentifier.Slot(slot) -> slot.Named
                                    | _ -> false

                                let attribute =
                                    match statement.Attribute with
                                    | PatternIdentifier.Slot(slot) -> slot.Named
                                    | _ -> false

                                let value =
                                    match statement.Value with
                                    | PatternValue.Slot(slot) -> slot.Named
                                    | _ -> false

                                entity || attribute || value)
                            statements.PatternStatements

                    Ok(WanderValue.Bool(result))
                | value -> error $"Unexpected value passed to Pattern.extracts - {value}." None)
        )
    )

let datasetLib<'t> =
    WanderValue.Record(
        Map
            [ ("count", countFunction)
              ("extract", extractFunction)
              ("extracts", extractsFunction)
              ("isDataset", isDatasetFunction) ]
    )
