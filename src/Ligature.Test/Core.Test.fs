// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.InMemoryNetwork.Test

open Expecto
open Ligature.Model
open Ligature.Core

let elementPattern e = TermPattern.Term(Term e)

[<Tests>]
let tests =
    testList
        "Core Test Suite"
        [
          // testCase "contains"
          // <| fun _ ->
          //     let testCases = [
          //       (Set.ofList [(Element "e", Element "a", Value.Element(Element "v"))],
          //       Set.ofList [(Element "e", Element "a", Value.Element(Element "v"))]
          //       , true)
          //     ]

          //     List.iter (fun (testNetwork, source, res) ->
          //       Expect.equal (contains testNetwork source) res  "")
          //       testCases

          testCase "matches"
          <| fun _ ->
              let testCases =
                  [ ((elementPattern "e", elementPattern "a", Value.Element(Element "v")),
                     Set.ofList [ (elementPattern "e", elementPattern "a", Value.Element(Element "v")) ],
                     Set.ofList [ Map.empty ])

                    // (Set.ofList [(elementPattern "e", elementPattern "a", ValuePattern.Element(Element "v"))],
                    // Set.ofList [(Element "e", Element "a", Value.Element(Element "v"))]
                    // , true)
                    ]

              List.iter
                  (fun (testNetwork, source, res) -> Expect.equal (singleMatch testNetwork source) res "")
                  testCases

          // testCase "empty filter"
          //   <| fun _ ->
          //       let result = filter Set.empty Set.empty
          //       Expect.equal result Set.empty ""
          //   testCase "empty pattern"
          //   <| fun _ ->
          //       let result =
          //           filter Set.empty (Set.ofList [ (Element "e", Element "a", Value.Element(Element "v")) ])

          //       Expect.equal result Set.empty ""
          //   testCase "empty network"
          //   <| fun _ ->
          //       let result =
          //           filter
          //               (Set.ofList
          //                   [ (ElementPattern.Element(Element "e"),
          //                      ElementPattern.Element(Element "c"),
          //                      ValuePattern.Element(Element "e")) ])
          //               Set.empty

          //       Expect.equal result Set.empty ""
          //   testCase "wildcard pattern"
          //   <| fun _ ->
          //       let result =
          //           filter
          //               (Set.ofList
          //                   [ (ElementPattern.Variable(Variable "e"),
          //                      ElementPattern.Variable(Variable "a"),
          //                      ValuePattern.Variable(Variable "v")) ])
          //               (Set.ofList [ (Element "e", Element "a", Value.Element(Element "v")) ])

          //       Expect.equal result (Set.ofList [ (Element "e", Element "a", Value.Element(Element "v")) ]) ""
          //   testCase "not matching pattern"
          //   <| fun _ ->
          //       let result =
          //           filter
          //               (Set.ofList
          //                   [ (ElementPattern.Variable(Variable "e"),
          //                      ElementPattern.Variable(Variable "a"),
          //                      ValuePattern.Element(Element "v2")) ])
          //               (Set.ofList [ (Element "e", Element "a", Value.Element(Element "v")) ])

          //       Expect.equal result Set.empty ""
          ]
