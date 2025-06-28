// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Http

open Wander.Main
open Wander.Fns
open Wander.Model
open System
open Ligature.Model
open Ligature.InMemoryStore
open Falco
open Falco.Routing
open Microsoft.AspNetCore.Builder
open Store

let createEndpoints (store: ILigatureStore) =
    [ post "/" (fun ctx ->
          task {
              let! body = Request.getBodyString ctx

              return!
                  match run (Wander.Library.stdFns store) Map.empty Map.empty body with
                  | Ok result -> Response.ofPlainText (printAny result) ctx
                  | Error err -> Response.ofPlainText err.UserMessage ctx
          }) ]

let wapp = WebApplication.Create()

let path = Environment.GetEnvironmentVariable "LIGATURE_HOME" + "/store"
let store = InMemoryStore() //new LigatureStore(Some path) 

wapp.UseRouting().UseFalco(createEndpoints store).Run()
