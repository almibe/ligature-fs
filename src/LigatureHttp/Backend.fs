// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Http.Backend

open Giraffe
open Ligature
open Ligature.Wander.Main
open Microsoft.AspNetCore.Http

let handleError (ctx: HttpContext) err = ctx.WriteStringAsync(err.UserMessage) //TODO return error code, not 200

let runWander instance : HttpHandler =
    handleContext (fun ctx ->
        let x = ctx.ReadBodyFromRequestAsync ()
        let bindings = Wander.Preludes.instancePrelude instance
        let res = run x.Result bindings
        match res with
        | Ok(res) -> ctx.WriteStringAsync (Wander.Model.prettyPrint res)
        | Error(err) -> handleError ctx err)

let backendWebApp instance =
    choose
        [ POST
          >=> choose
              [ route "/wander" >=> runWander instance ] ]
