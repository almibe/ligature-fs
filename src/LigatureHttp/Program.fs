// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.DevServer

open Wander.Main
open Wander.Fns
open Wander.Model
open System
open Ligature.Model
open Ligature.InMemoryStore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Microsoft.AspNetCore.Http
open System.IO
open Giraffe.ViewEngine
open Wander.Fns.Html

//let path = Environment.GetEnvironmentVariable "LIGATURE_HOME" + "/store"
let store = InMemoryStore() //new LigatureStore(Some path)

let wanderHandler: HttpHandler =
    fun (next: HttpFunc) (ctx: HttpContext) ->
        if HttpMethods.IsPost ctx.Request.Method then
            task {
                let! script = ctx.ReadBodyFromRequestAsync false

                return!
                    match run (Wander.Library.stdFns store) Map.empty script with
                    | Ok result -> ctx.WriteTextAsync(printExpression result)
                    | Error err -> ctx.WriteTextAsync err.UserMessage
            }
        else
            failwith "TODO"

let createEndpoints (store: ILigatureStore) =
    choose [ POST >=> wanderHandler ]

let wapp = WebApplication.Create()

type Startup() =
    member __.ConfigureServices(services: IServiceCollection) =
        // Register default Giraffe dependencies
        services.AddGiraffe() |> ignore

    member __.Configure (app: IApplicationBuilder) (env: IHostEnvironment) (loggerFactory: ILoggerFactory) =
        // Add Giraffe to the ASP.NET Core pipeline
        app.UseGiraffe(createEndpoints (InMemoryStore()))

[<EntryPoint>]
let main _ =
    Host
        .CreateDefaultBuilder()
        .ConfigureWebHostDefaults(fun webHostBuilder -> webHostBuilder.UseStartup<Startup>() |> ignore)
        .Build()
        .Run()

    0
