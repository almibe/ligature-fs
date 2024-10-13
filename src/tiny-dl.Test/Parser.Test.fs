// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Parser.Test

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
          testCase "Concept Equiv"
          <| fun _ -> Expect.equal (parse "X ≡ Y") (Ok [ Node.ConceptDefinition("X", "Y") ]) ""
          testCase "Concept Inclusion"
          <| fun _ -> Expect.equal (parse "X ⊑ Y") (Ok [ Node.ConceptInclusion("X", "Y") ]) "" ]
