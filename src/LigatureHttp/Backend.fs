// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Http.Backend

open Giraffe
open Ligature
open Ligature.InMemory
open Ligature.Wander.Main
open Microsoft.AspNetCore.Http

//TODO should be a param or service and not a local
let instance: ILigature = LigatureInMemory()

let handleError (ctx: HttpContext) err = ctx.WriteStringAsync(err.UserMessage) //TODO return error code, not 200

let runWander () : HttpHandler =
    handleContext (fun ctx ->
        let x = ctx.ReadBodyFromRequestAsync ()
        let bindings = Wander.Preludes.instancePrelude instance
        let res = run x.Result bindings
        match res with
        | Ok(res) -> ctx.WriteStringAsync (sprintf "%A" res)
        | Error(err) -> handleError ctx err)

let backendWebApp () =
    choose
        [ POST
          >=> choose
              [ route "/wander" >=> runWander () ] ]
