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
        [ testCase "Parse empty script" <| fun _ -> Expect.equal (parse "") (Ok []) ""
          testCase "Parse single individual"
          <| fun _ -> Expect.equal (parse "x:Y") (Ok([ Node.Extension("x", "Y") ])) ""
          testCase "Parse single individual with Negatation"
          <| fun _ -> Expect.equal (parse "x:¬Y") (Ok([ Node.NonExtension("x", "Y") ])) ""
          testCase "Don't allow anonymous concepts"
          <| fun _ -> Expect.isError (parse "x:Y⊓Z") "" ]
//   testCase "Parse single individual with multiple Conjunctions"
//   <| fun _ ->
//       Expect.equal
//            (parse "w:X⊓Y⊓Z")
//            (Ok([ Node.UnaryPredicate("w", Node.BinaryOperator(Node.Name("X"), Conjuntion, Node.BinaryOperator(Node.Name("Y"), Conjuntion, Node.Name("Z")))) ]))
//       "" ]
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
