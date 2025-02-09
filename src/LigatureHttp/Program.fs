// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.ZMQ.Main

open Wander.Main
open System
open Ligature.Model
open Wander.Actions
open Wander.Model
open Falco
open Falco.Routing
open Microsoft.AspNetCore.Builder
open Ligature.Lmdb

let createEndpoints (store: IStore) =
    [ get "/" (fun ctx ->
          let message = Seq.fold (fun state value -> state + " " + value) "[" (store.Networks()) + "]\n"
          Response.ofPlainText message ctx)
      post "/{name}" (fun ctx ->
          let route = Request.getRoute ctx
          let name = route.GetString "name"
          task {
            let! body = Request.getBodyString ctx
            let _ = store.AddNetwork name
            match run (createStoreActions store Wander.Library.stdActions) List.empty body with
            | _ -> Response.ofPlainText "" ctx
          })
      delete "/{name}" (fun ctx ->
          let route = Request.getRoute ctx
          let name = route.GetString "name"
          store.RemoveNetwork name
          Response.ofPlainText "" ctx) ]

let wapp = WebApplication.Create()

wapp
    .UseRouting()
    .UseFalco(createEndpoints (createInMemoryStore ()))
    // ^-- activate Falco endpoint source
    .Run()
