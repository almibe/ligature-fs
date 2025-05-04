// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Interpreter.Test

open Expecto
open Ligature.Model
open Ligature.Core


[<Tests>]
let tests =
    testList
        "Interpreter Tests"
        [ testCase "Empty Definitions to Map test"
          <| fun _ ->
              let input = Set.empty
              let output = tBoxToMap input
              Expect.equal output (Some Map.empty) ""

          testCase "Single Definition to Map test"
          <| fun _ ->
              let input =
                  Set.ofList
                      [ Definition.Equivalent(ConceptExpr.AtomicConcept(Term "a"), ConceptExpr.AtomicConcept(Term "A")) ]

              let output = tBoxToMap input
              Expect.equal output (Some(Map.ofList [ Term "a", ConceptExpr.AtomicConcept(Term "A") ])) ""

          testCase "Multiple Definitions to Map test"
          <| fun _ ->
              let input =
                  Set.ofList
                      [ Definition.Equivalent(ConceptExpr.AtomicConcept(Term "a"), ConceptExpr.AtomicConcept(Term "A"))
                        Definition.Implies(
                            ConceptExpr.AtomicConcept(Term "b"),
                            ConceptExpr.And [ ConceptExpr.AtomicConcept(Term "A"); ConceptExpr.AtomicConcept(Term "B") ]
                        )
                        Definition.Equivalent(ConceptExpr.AtomicConcept(Term "c"), ConceptExpr.AtomicConcept(Term "A")) ]

              let output = tBoxToMap input

              Expect.equal
                  output
                  (Some(
                      Map.ofList
                          [ Term "a", ConceptExpr.AtomicConcept(Term "A")
                            Term "c", ConceptExpr.AtomicConcept(Term "A")
                            Term "b",
                            ConceptExpr.And [ ConceptExpr.AtomicConcept(Term "A"); ConceptExpr.AtomicConcept(Term "B") ] ]
                  ))
                  ""

          //   testCase "matches"
          //   <| fun _ ->
          //       let testCases =
          //           [ ((elementPattern "e", elementPattern "a", Value.Term(Term "v")),
          //              Set.ofList [ (elementPattern "e", elementPattern "a", Value.Term(Term "v")) ],
          //              Set.ofList [ Map.empty ])

          //             // (Set.ofList [(elementPattern "e", elementPattern "a", ValuePattern.Term(Term "v"))],
          //             // Set.ofList [(Term "e", Term "a", Value.Term(Term "v"))]
          //             // , true)
          //             ]

          //       List.iter
          //           (fun (testNetwork, source, res) -> Expect.equal (singleMatch testNetwork source) res "")
          //           testCases

          // testCase "empty filter"
          //   <| fun _ ->
          //       let result = filter Set.empty Set.empty
          //       Expect.equal result Set.empty ""
          //   testCase "empty pattern"
          //   <| fun _ ->
          //       let result =
          //           filter Set.empty (Set.ofList [ (Term "e", Term "a", Value.Term(Term "v")) ])

          //       Expect.equal result Set.empty ""
          //   testCase "empty network"
          //   <| fun _ ->
          //       let result =
          //           filter
          //               (Set.ofList
          //                   [ (TermPattern.Term(Term "e"),
          //                      TermPattern.Term(Term "c"),
          //                      ValuePattern.Term(Term "e")) ])
          //               Set.empty

          //       Expect.equal result Set.empty ""
          //   testCase "wildcard pattern"
          //   <| fun _ ->
          //       let result =
          //           filter
          //               (Set.ofList
          //                   [ (TermPattern.Slot(Slot "e"),
          //                      TermPattern.Slot(Slot "a"),
          //                      ValuePattern.Slot(Slot "v")) ])
          //               (Set.ofList [ (Term "e", Term "a", Value.Term(Term "v")) ])

          //       Expect.equal result (Set.ofList [ (Term "e", Term "a", Value.Term(Term "v")) ]) ""
          //   testCase "not matching pattern"
          //   <| fun _ ->
          //       let result =
          //           filter
          //               (Set.ofList
          //                   [ (TermPattern.Slot(Slot "e"),
          //                      TermPattern.Slot(Slot "a"),
          //                      ValuePattern.Term(Term "v2")) ])
          //               (Set.ofList [ (Term "e", Term "a", Value.Term(Term "v")) ])

          //       Expect.equal result Set.empty ""
          ]
