// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Infer.Test

open Expecto
open TinyDL.Main
open TinyDL.Interpreter

[<Tests>]
let tests =
    testList
        "Check Tests"
        [ 
          // testCase "Call interpret on empty arguments"
          // <| fun _ ->
          //     Expect.equal
          //         (interpret emptyKB)
          //         (Ok
          //             { Domain = Set.empty
          //               Concepts = Map.empty
          //               Roles = Map.empty })
          //         ""
          testCase "Call interpret with empty TBox"
          <| fun _ ->
              Expect.equal
                  (interpret (Set.empty, (Set.ofList [ UnaryPredicate { symbol = "betty"; concept = "Cat" } ])))
                  (Ok
                      { Domain = Set.ofList [ Symbol "betty" ]
                        Concepts = Map.ofList [ (Symbol "Cat", Set.ofList [ Symbol "betty" ]) ]
                        Roles = Map.empty })
                  ""
          // testCase "Call interpret with empty ABox"
          // <| fun _ ->
          //     Expect.equal
          //         (interpret (
          //             (Set.ofList
          //                 [ Definition
          //                       { left = "DomesticCat"
          //                         right = AtomicConcept "HouseCat" } ]),
          //             emptyABox
          //         ))
          //         (Ok
          //             { Domain = Set.ofList []
          //               Concepts = Map.ofList [ (Symbol "DomesticCat", Set.empty); (Symbol "HouseCat", Set.empty) ]
          //               Roles = Map.empty })
          //         ""
          testCase "Read multiple unary predicates with one individual"
          <| fun _ ->
              Expect.equal
                  (eval "betty: Cat, betty: HouseCat")
                  (Ok
                      { Domain = Set.ofList [ Symbol "betty" ]
                        Concepts =
                          Map.ofList
                              [ (Symbol "Cat", Set.ofList [ Symbol "betty" ])
                                (Symbol "HouseCat", Set.ofList [ Symbol "betty" ]) ]
                        Roles = Map.empty })
                  ""
          testCase "Read multiple unary predicates"
          <| fun _ ->
              Expect.equal
                  (eval "betty: Cat, don: Cat")
                  (Ok
                      { Domain = Set.ofList [ Symbol "betty"; Symbol "don" ]
                        Concepts = Map.ofList [ (Symbol "Cat", Set.ofList [ Symbol "betty"; Symbol "don" ]) ]
                        Roles = Map.empty })
                  ""
          testCase "Binary predicate"
          <| fun _ ->
              Expect.equal
                  (eval "(betty, 11lbs): weight")
                  (Ok
                      { Domain = Set.ofList [ Symbol "betty"; Symbol "11lbs" ]
                        Concepts = Map.empty
                        Roles = Map.ofList [ ("weight", Set.ofList [ ("betty", "11lbs") ]) ] })
                  "" ]
          // testCase "Basic Definition"
          // <| fun _ ->
          //     Expect.equal
          //         (eval "Cat ≡ HouseCat, betty: Cat")
          //         (Ok
          //             { Domain = Set.ofList [ Symbol "betty" ]
          //               Concepts =
          //                 Map.ofList
          //                     [ (Symbol "Cat", Set.ofList [ Symbol "betty" ])
          //                       (Symbol "HouseCat", Set.ofList [ Symbol "betty" ]) ]
          //               Roles = Map.empty })
          //         "" ]
          // testCase "Basic Inclusion"
          // <| fun _ ->
          //     Expect.equal
          //         (eval "Cat ⊑ Animal, betty: Cat")
          //         (Ok
          //             { Domain = Set.ofList [ Symbol "betty" ]
          //               Concepts =
          //                 Map.ofList
          //                     [ (Symbol "Cat", Set.ofList [ Symbol "betty" ])
          //                       (Symbol "Animal", Set.ofList [ Symbol "betty" ]) ]
          //               Roles = Map.empty })
          //         ""
          // testCase "Basic Existential Restriction"
          // <| fun _ ->
          //     Expect.equal
          //         (eval "Cat ⊑ ∃weight.Weight, (betty, 11lbs): weight")
          //         (Ok
          //             { Domain = Set.ofList [ Symbol "betty"; Symbol "11lbs" ]
          //               Concepts =
          //                 Map.ofList
          //                     [ (Symbol "Cat", Set.ofList [ Symbol "betty" ])
          //                       (Symbol "Weight", Set.ofList [ Symbol "11lbs" ]) ]
          //               Roles = Map.ofList [ (Symbol "weight", Set.ofList [ (Symbol "betty", Symbol "11lbs") ]) ] })
          //         "" ]
