// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Parser.Test

open Expecto
open TinyDL.Tokenizer
open TinyDL.NewParser
open TinyDL.Main

let parse (input: string) =
    match tokenize input with
    | Ok res -> parse res
    | _ -> failwith "Error"

[<Tests>]
let tests =
    testList
        "Parser Tests"
        [ testCase "Parse empty script"
          <| fun _ -> Expect.equal (parse "") (Ok(Set.empty, Set.empty)) ""
          testCase "Parse single individual"
          <| fun _ ->
              Expect.equal
                  (parse "x:Y")
                  (Ok(
                      Set.empty,
                      Set.ofList
                          [ UnaryPredicate
                                { symbol = "x"
                                  concept = AtomicConcept "Y" } ]
                  ))
                  ""
          testCase "Parse single individual with Negatation"
          <| fun _ ->
              Expect.equal
                  (parse "x:¬Y")
                  (Ok(
                      Set.empty,
                      Set.ofList
                          [ UnaryPredicate
                                { symbol = "x"
                                  concept = Not { concept = AtomicConcept "Y" } } ]
                  ))
                  ""
          testCase "Parse single individual with Conjunction"
          <| fun _ ->
              Expect.equal
                  (parse "x:Y⊓Z")
                  (Ok(
                      Set.empty,
                      Set.ofList
                          [ UnaryPredicate
                                { symbol = "x"
                                  concept =
                                    Conjunction
                                        { left = AtomicConcept "Y"
                                          right = AtomicConcept "Z" } } ]
                  ))
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
