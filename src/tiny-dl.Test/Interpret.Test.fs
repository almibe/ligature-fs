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
                  (interpret (
                      Set.empty,
                      (Set.ofList
                          [ UnaryPredicate
                                { symbol = "betty"
                                  concept = AtomicConcept "Cat" } ])
                  ))
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
                      { Domain = Set.empty
                        Concepts = Map.ofList [ (Symbol "DomesticCat", Set.empty); (Symbol "HouseCat", Set.empty) ]
                        Roles = Map.empty })
                  "" ]
