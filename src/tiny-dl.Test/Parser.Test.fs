// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Parser.Test

open Expecto
open TinyDL.Tokenizer
open TinyDL.Parser
open TinyDL.Main

[<Tests>]
let tests =
    testList
        "Parser Tests"
        [ testCase "Parse empty script"
          <| fun _ -> Expect.equal (parse []) (Ok(Set.empty, Set.empty)) ""
          testCase "Parse single individual"
          <| fun _ ->
              Expect.equal
                  (parse [ Token.Name("x"); Token.Colon; Token.Name("Y") ])
                  (Ok(Set.empty, Set.ofList [ UnaryPredicate { symbol = "x"; concept = "Y" } ]))
                  "" ]
        //   testCase "Concept Equiv"
        //   <| fun _ ->
        //       Expect.equal
        //           (parse [ Token.Name("X"); Token.Definition; Token.Name("Y") ])
        //           (Ok(
        //               Set.ofList
        //                   [ Definition
        //                         { left = "X"
        //                           right = AtomicConcept "Y" } ],
        //               emptyABox
        //           ))
        //           "" ]
        //   testCase "Concept Inclusion"
        //   <| fun _ ->
        //       let tokens =
        //           match tokenize "X ⊑ Y" with
        //           | Ok res -> res
        //           | _ -> failwith "TODO"

        //       Expect.equal
        //           (parse tokens)
        //           (Ok(
        //               Set.ofList
        //                   [ Inclusion
        //                         { left = "X"
        //                           right = AtomicConcept "Y" } ],
        //               emptyABox
        //           ))
        //           "" ]
