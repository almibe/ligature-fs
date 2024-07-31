// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.ZMQ.Main

open Ligature.ZMQ.Config
open Ligature
open Ligature.Wander.Main
open NetMQ.Sockets
open NetMQ
open System
open Ligature.LigatureStore
open Ligature.LigatureStore.InMemoryStore
open Ligature.Wander.Interpreter

let rec serve (server: ResponseSocket) (store: LigatureStore) =
    let script = server.ReceiveFrameString()
    let res = run script emptyNetwork
    server.SendFrame(printResult res)
    serve server store

[<EntryPoint>]
let main args =
    let inmem = (args = [| "inmem" |])
    Console.WriteLine("Starting Ligature ZeroMQ.")
    let config = readConfig ()

    let store = if inmem then empty () else empty ()
    //let path = System.Environment.GetEnvironmentVariable "LIGATURE_HOME"
    //let store = //new LigatureSqlite(path + "\\sqlite\\store.db")
    //store.initialize () |> ignore
    //store

    use server = new ResponseSocket()
    server.Bind("tcp://localhost:4200")
    Console.WriteLine("Started on port 4200.")
    serve server store
    0
