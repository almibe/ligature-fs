// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.WebSockets.Main

open System
open System.Threading

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket

open Ligature.WebSockets.Config
open Ligature
open Ligature.Wander.Main
open Ligature.Wander.Interpreter
open Ligature.Main
open Ligature.Wander.Lib.Combinators

let ws (webSocket : WebSocket) (context: HttpContext) =
    socket {
        let mutable loop = true
        let mutable state = Set.empty

        while loop do
            let! msg = webSocket.read()

            match msg with
            | (Text, data, true) ->
                let script = UTF8.toString data
                let res = run state script 
                match res with
                | Ok(newState) -> state <- newState
                | _ -> ignore
                
                let res = res |> printResult

                let byteResponse =
                    res
                    |> System.Text.Encoding.ASCII.GetBytes
                    |> ByteSegment
                do! webSocket.send Text byteResponse true

            | (Close, _, _) ->
                let emptyResponse = [||] |> ByteSegment
                do! webSocket.send Close emptyResponse true
                loop <- false

            | _ -> ()
      }

let app =
  choose
    [ path "/websocket" >=> handShake ws ]

[<EntryPoint>]
let main argv = 
    let cts = new CancellationTokenSource()
    let conf = { defaultConfig with cancellationToken = cts.Token }
    let listening, server = startWebServerAsync conf app

    Async.Start(server, cts.Token)
    printfn "LigatureWebSockets started."
    Console.ReadKey true |> ignore

    cts.Cancel()

    0
