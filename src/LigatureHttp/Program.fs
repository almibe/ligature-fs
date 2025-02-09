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

let endpoints =
    [ get "/" (fun ctx ->
          let message = ""
          Response.ofPlainText message ctx)
      put "/{name}/" (fun ctx ->
          let route = Request.getRoute ctx
          let name = route.GetString "name"
          let message = sprintf "Hello %s" name
          Response.ofPlainText message ctx)
      post "/{name}/" (fun ctx ->
          let route = Request.getRoute ctx
          let name = route.GetString "name"
          let message = sprintf "Hello %s" name
          Response.ofPlainText message ctx)
      delete "/{name}/" (fun ctx ->
          let route = Request.getRoute ctx
          let name = route.GetString "name"
          let message = sprintf "Hello %s" name
          Response.ofPlainText message ctx) ]

let wapp = WebApplication.Create()

wapp
    .UseRouting()
    .UseFalco(endpoints)
    // ^-- activate Falco endpoint source
    .Run()
