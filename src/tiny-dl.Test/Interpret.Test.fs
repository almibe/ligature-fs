// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Infer.Test

open Expecto
open TinyDL.Main

[<Tests>]
let tests =
    testList
        "Check Tests"
        [ testCase "Call check on empty arguments"
          <| fun _ ->
              Expect.equal
                  (interpret emptyKB)
                  { Domain = Set.empty
                    Concepts = Map.empty
                    Roles = Map.empty }
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
                  { Domain = Set.empty
                    Concepts = Map.empty
                    Roles = Map.empty }
                  //(Map.ofList [ (InterpretationKey.Concept(Symbol "Cat"), Set.ofList [ Symbol "betty" ]) ])
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
                  { Domain = Set.empty
                    Concepts = Map.empty
                    Roles = Map.empty }
                  "" ]
