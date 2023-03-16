// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Lab.Main

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Giraffe.ViewEngine
open System.IO
open Ligature.Sqlite.Main
open Ligature.Lab.Config
open Ligature.Lab.Backend
open Ligature
open Microsoft.AspNetCore.Http

let config = readConfig ()

let instance = ligatureSqlite (InMemory)

let handleError (ctx: HttpContext) err = ctx.WriteStringAsync(err.userMessage) //TODO return error code, not 200

let datasets () =
    instance.AllDatasets()
    |> Result.map (fun s -> s |> List.map (fun ds -> datasetName ds))

let datasetList () =
    match datasets () with
    | Ok(ds) -> (List.map (fun ds -> p [] [ str ds ]) ds)
    | Error(err) -> [ p [] [ str "Error reading Datasets." ] ]

let webApp = backendWebApp ()

let configureApp (app: IApplicationBuilder) =
    app.UseStaticFiles() |> ignore
    app.UseDefaultFiles() |> ignore
    app.UseGiraffe webApp

let configureServices (services: IServiceCollection) = services.AddGiraffe() |> ignore

[<EntryPoint>]
let main _ =
    let contentRoot = Directory.GetCurrentDirectory()
    let webRoot = Path.Combine(contentRoot, "WebRoot")

    Host
        .CreateDefaultBuilder()
        .ConfigureWebHostDefaults(fun webHostBuilder ->
            webHostBuilder
                .UseUrls(config.url)
                .UseContentRoot(contentRoot)
                .UseWebRoot(webRoot)
                .Configure(configureApp)
                .ConfigureServices(configureServices)
            |> ignore)
        .Build()
        .Run()

    0
