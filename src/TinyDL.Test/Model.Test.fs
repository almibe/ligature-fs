// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Test.Model

open Expecto
open Ligature.Model
open Ligature.Core
open TinyDL.Model

[<Tests>]
let tests =
    testList
        "Network to Model Test Suite"
        [ testCase "pass empty network"
          <| fun _ ->
              let result = networkToModel Set.empty
              Expect.equal result Set.empty ""
          testCase "A ≡ B"
          <| fun _ ->
              let result =
                  networkToModel (
                      Set.ofList
                          [ (Term "A", Term "tiny-dl.≡", Value.Term(Term "B"))
                            (Term "B", Term ":", Value.Term(Term "tiny-dl.ConceptName")) ]
                  )

              Expect.equal
                  result
                  (Set.ofList [ ConceptDef.ConceptEquiv(Term "A", ConceptExp.ConceptName(Term "B")) ])
                  ""
          testCase "A ≡ B ⊓ C"
          <| fun _ ->
              let result =
                  networkToModel (
                      Set.ofList
                          [ (Term "A", Term "tiny-dl.≡", Value.Term(Term "_1"))
                            (Term "_1", Term ":", Value.Term(Term "tiny-dl.Conjunction"))
                            (Term "_1", Term "tiny-dl.conjunct", Value.Term(Term "B"))
                            (Term "_1", Term "tiny-dl.conjunct", Value.Term(Term "C"))
                            (Term "B", Term ":", Value.Term(Term "tiny-dl.ConceptName"))
                            (Term "C", Term ":", Value.Term(Term "tiny-dl.ConceptName")) ]
                  )

              Expect.equal
                  result
                  (Set.ofList
                      [ ConceptDef.ConceptEquiv(
                            Term "A",
                            ConceptExp.ConceptConjection(
                                Set.ofList [ ConceptExp.ConceptName(Term "B"); ConceptExp.ConceptName(Term "C") ]
                            )
                        ) ])
                  "" ]
