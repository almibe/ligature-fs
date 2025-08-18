// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.DevServer

open Wander.Main
open Wander.Fns
open Wander.Model
open Ligature.Model
open System
open Log
open Suave
open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket
open Suave.Filters
open Suave.Operators
open Suave.Successful

let storePath = Environment.GetEnvironmentVariable "LIGATURE_HOME" + "/store"
let store = new LogStore(Some storePath)

let ws (webSocket : WebSocket) (context: HttpContext) =
    socket {
        let mutable loop = true

        while loop do
            let! msg = webSocket.read()

            match msg with
            | Text, data, true ->
                let str = UTF8.toString data
                let response = sprintf "response to %s" str
                let byteResponse =
                    response
                    |> System.Text.Encoding.ASCII.GetBytes
                    |> ByteSegment
                do! webSocket.send Text byteResponse true

            | Close, _, _ ->
                let emptyResponse = [||] |> ByteSegment
                do! webSocket.send Close emptyResponse true
                loop <- false

            | _ -> ()
    }

// let wanderHandler: HttpHandler =
//     fun (next: HttpFunc) (ctx: HttpContext) ->
//         if HttpMethods.IsPost ctx.Request.Method then
//             task {
//                 let! script = ctx.ReadBodyFromRequestAsync false

//                 return!
//                     match run (Wander.Library.stdFns store) Map.empty script with
//                     | Ok result -> ctx.WriteTextAsync(printExpression result)
//                     | Error err -> ctx.WriteTextAsync err.UserMessage
//             }
//         else
//             failwith "TODO"

let app : WebPart =
    choose [
        path "/ws" >=> handShake ws ]

[<EntryPoint>]
let main _ =
    startWebServer defaultConfig app

    0
