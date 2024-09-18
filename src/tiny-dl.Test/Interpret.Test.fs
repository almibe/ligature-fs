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
        [ testCase "Call check on empty arguments"
          <| fun _ ->
              Expect.equal
                  (interpret emptyKB)
                  (Ok
                      { Domain = Set.empty
                        Concepts = Map.empty
                        Roles = Map.empty })
                  ""
          testCase "Call check with empty TBox"
          <| fun _ ->
              Expect.equal
                  (interpret (Set.empty, (Set.ofList [ UnaryPredicate { symbol = "betty"; concept = "Cat" } ])))
                  (Ok
                      { Domain = Set.ofList [ Symbol "betty"; Symbol "Cat" ]
                        Concepts = Map.ofList [ (Symbol "Cat", Set.ofList [ Symbol "betty" ]) ]
                        Roles = Map.empty })
                  ""
          testCase "Call check with empty ABox"
          <| fun _ ->
              Expect.equal
                  (interpret (
                      (Set.ofList
                          [ Equivalence
                                { left = AtomicConcept "DomesticCat"
                                  right = AtomicConcept "HouseCat" } ]),
                      emptyABox
                  ))
                  (Ok
                      { Domain = Set.ofList [ Symbol "DomesticCat"; Symbol "HouseCat" ]
                        Concepts = Map.ofList [ (Symbol "DomesticCat", Set.empty); (Symbol "HouseCat", Set.empty) ]
                        Roles = Map.empty })
                  ""
          testCase "Read multiple unary predicates with one individual"
          <| fun _ ->
              Expect.equal
                  (eval "betty: Cat, betty: HouseCat")
                  (Ok
                      { Domain = Set.ofList [ Symbol "betty"; Symbol "Cat"; Symbol "HouseCat" ]
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
                      { Domain = Set.ofList [ Symbol "betty"; Symbol "Cat"; Symbol "don" ]
                        Concepts = Map.ofList [ (Symbol "Cat", Set.ofList [ Symbol "betty"; Symbol "don" ]) ]
                        Roles = Map.empty })
                  ""
          testCase "Basic Equiv"
          <| fun _ ->
              Expect.equal
                  (eval "Cat ≡ HouseCat, betty: Cat")
                  (Ok
                      { Domain = Set.ofList [ Symbol "betty"; Symbol "Cat"; Symbol "HouseCat" ]
                        Concepts =
                          Map.ofList
                              [ (Symbol "Cat", Set.ofList [ Symbol "betty" ])
                                (Symbol "HouseCat", Set.ofList [ Symbol "betty" ]) ]
                        Roles = Map.empty })
                  "" ]
