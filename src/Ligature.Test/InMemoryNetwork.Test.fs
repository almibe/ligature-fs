// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.InMemoryNetwork.Test

open Expecto
open Ligature.Main
open Ligature.InMemoryNetwork

[<Tests>]
let tests =
    testList
        "Network Suite"
        [ testCase "read empty Network"
          <| fun _ ->
              let network: Network = Set.empty
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
          //   testCase "empty call to educe"
          //   <| fun _ -> Expect.equal (educeNetworkNetwork (Set.empty.Write()) (Set.empty.Write())) (Set.empty) ""
          //   testCase "calling educe on same triple"
          //   <| fun _ ->
          //       Expect.equal
          //           (educeTripleTriple
          //               (PatternName.Name(Name("a")), PatternName.Name(Name("b")), Value.Name(Name("c")))
          //               (PatternName.Name(Name("a")), PatternName.Name(Name("b")), Value.Name(Name("c"))))
          //           (Some(Map.empty))
          //           ""
          //   testCase "calling educe on non-matching triples"
          //   <| fun _ ->
          //       Expect.equal
          //           (educeTripleTriple
          //               (PatternName.Name(Name("e")), PatternName.Name(Name("b")), Value.Name(Name("c")))
          //               (PatternName.Name(Name("a")), PatternName.Name(Name("b")), Value.Name(Name("c"))))
          //           (None)
          //           ""
          //   testCase "calling educe on simple matching triple"
          //   <| fun _ ->
          //       Expect.equal
          //           (educeTripleTriple
          //               (PatternName.Name(Name("e")), PatternName.Name(Name("a")), Value.Name(Name("v")))
          //               (PatternName.Slot(Slot(Some("test"))), PatternName.Name(Name("a")), Value.Name(Name("v"))))
          //           (Some(Map [ ("test", Value.Name(Name("e"))) ]))
          //   "" ]
          ]
