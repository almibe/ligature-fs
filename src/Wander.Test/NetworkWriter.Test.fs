// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.NetworkWriter.Test

open Expecto
open Wander.Tokenizer
open Wander.Model
open Ligature.Core
open Ligature.Model

[<Tests>]
let tests = testList "Network Writer Tests" []
// [ testCase "Write empty store"
//   <| fun _ -> Expect.equal (writeStore (newInMemoryEngine ())) "" ""
//   testCase "Write store with single empty network"
//   <| fun _ ->
//       let store = newInMemoryEngine ()
//       ignore <| store.AddNetwork(NetworkName "test")
//       Expect.equal (writeStore store) "let test {}\n" ""
//   testCase "Write store with single network"
//   <| fun _ ->
//       let store = newInMemoryEngine ()

//       ignore
//       <| store.AddEntries
//           (NetworkName "test")
//           (Set.ofList
//               [ Entry.Extends
//                     { element = Element "element"
//                       concept = Element "Concept" } ])

//       Expect.equal (writeStore store) "let test { element : Concept,}\n" "" ]
