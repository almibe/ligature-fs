// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Giraffe.ViewEngine
open System.IO
open Ligature.Sqlite.Main
open Ligature.Lig.Write
open Ligature
open Microsoft.Extensions.Logging
open Microsoft.AspNetCore.Http

let instance = ligatureSqlite(InMemory)

let handleError (ctx: HttpContext) err =
    ctx.WriteStringAsync(err.userMessage) //TODO return error code, not 200

let datasets () = 
    instance.AllDatasets ()
    |> Result.map (fun s -> s |> List.map (fun ds -> readDataset ds))

let datasetList () =
    match datasets () with
    | Ok(ds) -> (List.map (fun ds -> p [] [ str ds]) ds)
    | Error(err) -> [ p [] [ str "Error reading Datasets."]]

let datasetsPage = html [] [
        head [] [
            title [] [ str "Ligature Lab" ]
        ]
        body [] (List.append [
            h1 [] [ str "Ligature Lab" ]
            h2 [] [ str "Datasets:"]
        ] (datasetList ()))
    ]

let webApp =
    choose [
        route "/" >=> htmlView datasetsPage ]

let configureApp (app : IApplicationBuilder) =
    app.UseStaticFiles() |> ignore
    app.UseDefaultFiles() |> ignore
    app.UseGiraffe webApp

let configureServices (services : IServiceCollection) =
    services.AddGiraffe() |> ignore

[<EntryPoint>]
let main _ =
    let contentRoot = Directory.GetCurrentDirectory()
    let webRoot     = Path.Combine(contentRoot, "WebRoot")
    Host.CreateDefaultBuilder()
        .ConfigureWebHostDefaults(
            fun webHostBuilder ->
                webHostBuilder
                    .UseContentRoot(contentRoot)
                    .UseWebRoot(webRoot)
                    .Configure(configureApp)
                    .ConfigureServices(configureServices)
                    |> ignore)
        .Build()
        .Run()
    0
