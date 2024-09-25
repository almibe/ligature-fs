// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.InMemoryNetwork.Test

open Expecto
open Ligature.Main
open Ligature.LigatureStore

[<Tests>]
let tests =
    testList
        "Network Suite"
        [ testCase "read empty Network"
          <| fun _ ->
              let network: ABox = Set.empty
              Expect.equal (network) (Set.empty) ""
          //   testCase "count Network"
          //   <| fun _ ->
          //       let network: Network = Set.empty
          //       Expect.equal (network.Count()) (0) ""
          //   testCase "union two Networks"
          //   <| fun _ ->
          //       let left: Network = Set.empty
          //       let right: Network = Set.empty
          //       Expect.equal (left.Union(right)) (InMemoryNetwork(Set [])) ""
          //   testCase "empty call to match"
          //   <| fun _ -> Expect.equal (matchNetworkNetwork (Set.empty.Write()) (Set.empty.Write())) (Set.empty) ""
          //   testCase "calling match on same triple"
          //   <| fun _ ->
          //       Expect.equal
          //           (matchTripleTriple
          //               (Pattern.Name(Name("a")), Pattern.Name(Name("b")), Value.Name(Name("c")))
          //               (Pattern.Name(Name("a")), Pattern.Name(Name("b")), Value.Name(Name("c"))))
          //           (Some(Map.empty))
          //           ""
          //   testCase "calling match on non-matching triples"
          //   <| fun _ ->
          //       Expect.equal
          //           (matchTripleTriple
          //               (Pattern.Name(Name("e")), Pattern.Name(Name("b")), Value.Name(Name("c")))
          //               (Pattern.Name(Name("a")), Pattern.Name(Name("b")), Value.Name(Name("c"))))
          //           (None)
          //           ""
          //   testCase "calling match on simple matching triple"
          //   <| fun _ ->
          //       Expect.equal
          //           (matchTripleTriple
          //               (Pattern.Name(Name("e")), Pattern.Name(Name("a")), Value.Name(Name("v")))
          //               (Pattern.Slot(Slot(Some("test"))), Pattern.Name(Name("a")), Value.Name(Name("v"))))
          //           (Some(Map [ ("test", Value.Name(Name("e"))) ]))
          //   "" ]
          ]
