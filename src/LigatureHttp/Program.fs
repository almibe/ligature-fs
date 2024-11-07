// // This Source Code Form is subject to the terms of the Mozilla Public
// // License, v. 2.0. If a copy of the MPL was not distributed with this
// // file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.WebSockets.Main

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.Http
open Giraffe
open Wander.Lib
open Wander.Main
open Ligature.InMemoryStore
open Ligature.Main

let networkToJson (network: Network) =
    Set.map
        (fun entry ->
            match entry with
            | Entry.Role { first = Symbol first
                           second = Symbol second
                           role = Symbol role } ->
                Map.ofList [ "type", "role"; "first", first; "second", second; "role", role ]
            | Entry.Extension { element = Symbol element
                                concept = Symbol concept } ->
                Map.ofList [ "type", "extension"; "element", element; "concept", concept ]
            | Entry.NonExtension { element = Symbol element
                                   concept = Symbol concept } ->
                Map.ofList [ "type", "nonextension"; "element", element; "concept", concept ])
        network

let toJson (store: LigatureStore) =
    let mutable result = Map.empty
    Set.iter (fun network -> result <- Map.add network (networkToJson (store.Read network)) result) (store.Networks())
    result

let handler next (ctx: HttpContext) =
    task {
        let! script = ctx.ReadBodyFromRequestAsync()
        let store = emptyInMemoryStore ()

        match run stdCommands store script with
        | Ok _ -> return! json (toJson store) next ctx
        | _ -> return! failwith "TODO"
    }

let webApp = POST >=> route "/" >=> handler

type Startup() =
    member __.ConfigureServices(services: IServiceCollection) =
        // Register default Giraffe dependencies
        //services.AddSingleton<Json.ISerializer>(Json.FsharpFriendlySerializer)
        services.AddGiraffe() |> ignore

    member __.Configure (app: IApplicationBuilder) (env: IHostEnvironment) (loggerFactory: ILoggerFactory) =
        // Add Giraffe to the ASP.NET Core pipeline
        app.UseGiraffe webApp

[<EntryPoint>]
let main _ =
    Host
        .CreateDefaultBuilder()
        .ConfigureWebHostDefaults(fun webHostBuilder -> webHostBuilder.UseStartup<Startup>() |> ignore)
        .Build()
        .Run()

    0
