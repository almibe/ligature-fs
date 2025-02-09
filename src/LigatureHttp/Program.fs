// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.ZMQ.Main

open Wander.Main
open System
open Ligature.Model
open Wander.Actions
open Wander.Model

// [<EntryPoint>]
// let main args =
//     Console.WriteLine("Starting Ligature HTTP.")
//     0

open Falco
open Falco.Routing
open Microsoft.AspNetCore.Builder
// ^-- this import adds many useful extensions

let endpoints =
    [
        get "/" (Response.ofPlainText "Hello World!")
        // ^-- associate GET / to plain text HttpHandler
    ]

let wapp = WebApplication.Create()

wapp.UseRouting()
    .UseFalco(endpoints)
    // ^-- activate Falco endpoint source
    .Run()