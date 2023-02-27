// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open Giraffe
open System.IO
open Ligature


let webApp =
    choose [
        GET >=> choose [
            route "/datasets" >=> text "All Datasets"
            route "/datasets/:datasetName/statements" >=> text "All Statements"
        ]
        POST >=> choose [
            route "/datasets/:datasetName" >=> text "Add Dataset"
            route "/datasets/:datasetName/statements" >=> text "Add Statements"
            route "/datasets/:datasetName/wander" >=> text "Query Dataset"
        ]
        DELETE >=> choose [
            route "/datasets/:datasetName" >=> text "Delete Dataset"
            route "/datasets/:datasetName/statements" >=> text "Delete Statements"
        ]
    ]

let configureApp (app : IApplicationBuilder) =
    app.UseGiraffe webApp

let configureServices (services : IServiceCollection) =
    services.AddGiraffe() |> ignore

[<EntryPoint>]
let main _ =
    Host.CreateDefaultBuilder()
        .ConfigureWebHostDefaults(
            fun webHostBuilder ->
                webHostBuilder
                    .Configure(configureApp)
                    .ConfigureServices(configureServices)
                    |> ignore)
        .Build()
        .Run()
    0
