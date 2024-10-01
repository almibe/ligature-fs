// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.InMemoryNetwork.Test

open Expecto
open Ligature.Main
open Ligature.LigatureStore
open Ligature.LigatureStore.InMemoryStore

[<Tests>]
let tests =
    testList
        "In Memory Store Suite"
        [ testCase "count empty store"
          <| fun _ ->
              let store = emptyInMemoryStore ()
              Expect.equal (store.Networks()) (Set.empty) ""
          testCase "add Network"
          <| fun _ ->
              let store = emptyInMemoryStore ()
              store.AddNetwork(NetworkName "test")
              Expect.equal (store.Networks()) (Set.ofList [ NetworkName "test" ]) ""
          testCase "add and remove Networks"
          <| fun _ ->
              let store = emptyInMemoryStore ()
              store.AddNetwork(NetworkName "test")
              store.AddNetwork(NetworkName "test2")
              store.AddNetwork(NetworkName "test3")
              store.RemoveNetwork(NetworkName "test2")
              store.RemoveNetwork(NetworkName "test")
              store.RemoveNetwork(NetworkName "test1")
              Expect.equal (store.Networks()) (Set.ofList [ NetworkName "test3" ]) "" ]
