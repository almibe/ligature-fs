// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.ZMQ.Main

open Ligature
open Wander.Main
open NetMQ.Sockets
open NetMQ
open System
open Wander.Interpreter
open Ligature.Main
open Wander.Commands
open Wander.Lib
open Ligature.InMemoryStore
open Wander.Model

let rec serve (server: ResponseSocket) =
    let script = server.ReceiveFrameString()

    match run stdCommands (emptyInMemoryStore ()) script with
    | Ok(Some(res)) -> server.SendFrame(prettyPrint res)
    | Error(err) -> server.SendFrame(err.UserMessage)

    serve server

[<EntryPoint>]
let main args =
    Console.WriteLine("Starting Ligature ZeroMQ.")
    use server = new ResponseSocket()
    server.Bind("tcp://localhost:4201")
    Console.WriteLine("Started on port 4201.")
    serve server
    0
