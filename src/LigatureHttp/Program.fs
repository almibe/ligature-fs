// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Http

open Wander.Main
open System
open Ligature.Model
open Wander.Actions
open Wander.Model
open Falco
open Falco.Routing
open Microsoft.AspNetCore.Builder
open Ligature.LigatureSqlite

let createEndpoints (store: LigatureSqlite) =
    [ 
      post "/" (fun ctx ->
          let route = Request.getRoute ctx
          
          task {
              let! body = Request.getBodyString ctx

              match run (createStoreActions store Wander.Library.stdActions) List.empty body with
              | Ok result -> Response.ofPlainText (printResult (Ok result)) ctx
              | Error err -> Response.ofPlainText err.UserMessage ctx
          })
      ]

let wapp = WebApplication.Create()

let store = LigatureSqlite ":memory:"
store.initialize()

wapp
    .UseRouting()
    .UseFalco(createEndpoints store)
    .Run()
