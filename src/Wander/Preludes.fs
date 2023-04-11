// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Preludes

open Ligature
open Ligature.Wander.Model

module private Boolean =
    let notFunction = Model.WanderValue.NativeFunction (
        new Model.NativeFunction((fun args ->
            match args.Head with
            | Model.Expression.Value(Model.WanderValue.Boolean(value)) -> Ok(Model.WanderValue.Boolean(not value))
            | _ -> error "" None)))

    let andFunction = Model.WanderValue.NativeFunction (
        new Model.NativeFunction((fun args ->
            match args.Head with
            | Model.Expression.Value(Model.WanderValue.Boolean(value)) -> Ok(Model.WanderValue.Boolean(not value))
            | _ -> error "" None)))

module private Instance = 
    let datasetsFunction (instance: ILigature) = Model.WanderValue.NativeFunction (
        new Model.NativeFunction(fun _ ->
            match instance.AllDatasets() with
            | Ok(datasets) ->
                let datasets = List.map (fun d -> Model.WanderValue.String(datasetName d)) datasets
                Ok(Model.Tuple(datasets))
            | Error(err) -> Error(err)))

    let addDataset (instance: ILigature) = WanderValue.NativeFunction (
        new Model.NativeFunction(fun args ->
            match args.Head with
            | Value(String(name)) -> 
                let dataset = Dataset(name)
                match instance.CreateDataset dataset with
                | Ok(_) -> Ok(Nothing)
                | Error(err) -> Error(err)
            | _ -> error "Could not add Dataset" None))

    let removeDataset (instance: ILigature) = WanderValue.NativeFunction (
        new Model.NativeFunction(fun args ->
            match args.Head with
            | Value(String(name)) -> 
                let dataset = Dataset(name)
                match instance.RemoveDataset dataset with
                | Ok(_) -> Ok(Nothing)
                | Error(err) -> Error(err)
            | _ -> error "Could not remove Dataset" None))

    let datasetExists (instance: ILigature) = WanderValue.NativeFunction (
        new Model.NativeFunction(fun args ->
            match args.Head with
            | Value(String(name)) -> 
                let dataset = Dataset(name)
                match instance.DatasetExists dataset with
                | Ok(result) -> Ok(Boolean(result))
                | Error(err) -> Error(err)
            | _ -> error "Could not check for Dataset" None))

    let query (instance: ILigature) = WanderValue.NativeFunction (
        new Model.NativeFunction(fun args ->
            let datasetName = args.Head
            let queryLambda = args.Tail.Head
            match (datasetName, queryLambda) with
            | (Value(String(name)), (Value(Lambda(_, _)))) -> error $"Could not run query on {name}." None
            | _ -> error "Improper arguments." None
        ))

let bindStandardLibrary bindings =
    Bindings.bind "not" Boolean.notFunction bindings
    |> Bindings.bind "and" Boolean.andFunction

let bindInstanceLevelFunctions instance bindings =
    bindings
    |> Bindings.bind "datasets" (Instance.datasetsFunction instance)
    |> Bindings.bind "createDataset" (Instance.addDataset instance)
    |> Bindings.bind "removeDataset" (Instance.removeDataset instance)
    |> Bindings.bind "datasetExists" (Instance.datasetExists instance)
    |> Bindings.bind "query" (Instance.query instance)
let standardPrelude () = 
    bindStandardLibrary (Bindings.newBindings ())

let instancePrelude (instance: ILigature): Bindings = 
    bindStandardLibrary (Bindings.newBindings ())
    |> bindInstanceLevelFunctions instance
