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
open Wander.Model
open Ligature.InMemoryStore

let handler next (ctx: HttpContext) = 
    task {
        let! script = ctx.ReadBodyFromRequestAsync()
        let store = emptyInMemoryStore ()
        match run stdCommands store script with
        | Ok _ -> return! json store next ctx
        | _ -> return! failwith "TODO"
    }

let webApp =
    POST >=> route "/" >=> handler

type Startup() =
    member __.ConfigureServices (services : IServiceCollection) =
        // Register default Giraffe dependencies
        //services.AddSingleton<Json.ISerializer>(Json.FsharpFriendlySerializer)
        services.AddGiraffe() |> ignore

    member __.Configure (app : IApplicationBuilder)
                        (env : IHostEnvironment)
                        (loggerFactory : ILoggerFactory) =
        // Add Giraffe to the ASP.NET Core pipeline
        app.UseGiraffe webApp

[<EntryPoint>]
let main _ =
    Host.CreateDefaultBuilder()
        .ConfigureWebHostDefaults(
            fun webHostBuilder ->
                webHostBuilder
                    .UseStartup<Startup>()
                    |> ignore)
        .Build()
        .Run()
    0

// open System
// open System.Threading

// open Ligature
// open Wander.Main
// open Wander.Interpreter
// open Ligature.Main
// open Wander.Commands
// open Ligature.InMemoryStore
// open Wander.Lib
// open Wander.Model

// // let handler =
// //     request (fun req ctx -> 
// //         req.form
// //         failwith "TODO")
//     // context (fun ctx -> 
//     //     ctx.request.
//     //     Successful.OK "test")
//     // socket {
//     //     let mutable loop = true
//     //     let mutable store = emptyInMemoryStore ()

//     //     while loop do
//     //         let! msg = webSocket.read ()

//     //         match msg with
//     //         | (Text, data, true) ->
//     //             let script = UTF8.toString data
//     //             let res = run stdCommands store script

//     //             let res: string = 
//     //                 match res with
//     //                 | Ok(Some(res)) -> prettyPrint res
//     //                 | _ -> failwith "TODO"

//     //             let byteResponse = res |> System.Text.Encoding.ASCII.GetBytes |> ByteSegment
//     //             do! webSocket.send Text byteResponse true

//     //         | (Close, _, _) ->
//     //             let emptyResponse = [||] |> ByteSegment
//     //             do! webSocket.send Close emptyResponse true
//     //             loop <- false

//     //         | _ -> ()
//     // }

// [<EntryPoint>]
// let main argv =
//     failwith "TODO"
