// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Read.Test

open Expecto
open TinyDL.Tokenizer
open TinyDL.NewParser

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
          //   testCase "Don't allow anonymous concepts"
          //   <| fun _ -> Expect.isError (parse "x:Y⊓Z") ""
          testCase "Concept Equiv"
          <| fun _ -> Expect.equal (parse "X ≡ Y") (Ok [ Node.ConceptDefinition("X", "Y") ]) ""
          testCase "Concept Inclusion"
          <| fun _ -> Expect.equal (parse "X ⊑ Y") (Ok [ Node.ConceptInclusion("X", "Y") ]) "" ]
