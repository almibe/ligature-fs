// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Bend.Preludes.Test

open Expecto
open Ligature.Bend.Model
open Ligature.Bend.Main
open Ligature.Bend.Identifier

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

let ident id =
    WanderValue.Identifier(
        match identifier id with
        | Ok v -> v
        | Error _ -> todo
    )

let bindings = standardPrelude ()

[<Tests>]
let tests =
    testList
        "Bool Prelude Test"
        [ testCase "Bool.not"
          <| fun _ ->
              let script = "Bool.not true"
              let result = run script bindings
              Expect.equal result (Ok(WanderValue.Bool(false))) ""
        ]
