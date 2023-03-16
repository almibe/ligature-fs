// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Lab.Backend

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open Giraffe
open System.IO
open Ligature
open Ligature.InMemory
open Ligature.Lig.Write
open Microsoft.Extensions.Logging
open Microsoft.AspNetCore.Http

//TODO should be a param or service and not a local
let instance: Ligature = LigatureInMemory()

let handleError (ctx: HttpContext) err = ctx.WriteStringAsync(err.userMessage) //TODO return error code, not 200

let allDatasets: HttpHandler =
    handleContext (fun ctx ->
        match instance.AllDatasets() with
        | Ok(ds) ->
            List.map datasetName ds
            |> List.fold (fun t d -> if t = "" then t + d else t + "\n" + d) ""
            |> ctx.WriteStringAsync
        | Error(err) -> handleError ctx err)

/// A helper function that handles validating Datasets.
let handleDatasetRequest (datasetName: string) request = todo

let addDataset (datasetName: string) : HttpHandler =
    handleContext (fun ctx ->
        match Dataset datasetName |> instance.CreateDataset with
        | Ok() -> ctx.WriteStringAsync $"Adding Dataset -- {datasetName}"
        | Error(error) -> ctx.WriteStringAsync $"Error - {error.userMessage}")

let removeDataset (datasetName: string) : HttpHandler =
    handleContext (fun ctx ->
        match Dataset datasetName |> instance.RemoveDataset with
        | Ok() -> ctx.WriteStringAsync $"Removing Dataset -- {datasetName}"
        | Error(error) -> ctx.WriteStringAsync $"Error - {error.userMessage}")

let allStatements (datasetName: string) : HttpHandler =
    handleContext (fun ctx ->
        let dataset = Dataset datasetName
        let res = instance.Query dataset (fun tx -> tx.AllStatements())

        match res with
        | Ok(statements) -> ctx.WriteStringAsync(writeLig statements)
        | Error(err) -> ctx.WriteStringAsync $"Error - {err.userMessage}")

let addStatements (datasetName: string) : HttpHandler =
    handleContext (fun ctx ->
        let dataset = Dataset datasetName

        let res =
            instance.Write dataset (fun tx ->
                let x = ctx.ReadBodyFromRequestAsync
                todo)

        match res with
        | Ok() -> ctx.WriteStringAsync ""
        | Error(err) -> handleError ctx err)

let backendWebApp () =
    choose
        [ GET
          >=> choose
              [ route "/datasets" >=> allDatasets
                routef "/datasets/%s/statements" (fun datasetName -> allStatements datasetName) ]
          POST
          >=> choose
              [ routef "/datasets/%s" (fun (datasetName) -> addDataset datasetName) //(fun datasetname -> addDataset datasetname)
                routef "/datasets/%s/statements" (fun (datasetName) -> addStatements datasetName)
                route "/datasets/:datasetName/wander" >=> text "todo" ]
          DELETE
          >=> choose
              [ routef "/datasets/%s" (fun (datasetName) -> removeDataset datasetName)
                route "/datasets/:datasetName/statements" >=> text "todo" ] ]
