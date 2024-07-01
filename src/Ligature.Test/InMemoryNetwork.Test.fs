// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.InMemoryNetwork.Test

open Expecto
open Ligature.Main
open Ligature.InMemoryNetwork

let ident id =
    match identifier id with
    | Ok id -> id
    | _ -> failwith "Error"

let vident id =
    match identifier id with
    | Ok id -> Value.Identifier id
    | _ -> failwith "Error"

[<Tests>]
let tests =
    testList
        "Network Suite"
        [ testCase "read empty Network"
          <| fun _ ->
              let network: Network = empty ()
              Expect.equal (network.Write()) (Set.empty) ""
          testCase "count Network"
          <| fun _ ->
              let network: Network = empty ()
              Expect.equal (network.Count()) (0) "" ]
