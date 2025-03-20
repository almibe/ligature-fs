// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Parser.Test

open Expecto
open Wander.Tokenizer
open Ligature.Model
open Wander.Model

let parse input =
    match tokenize input with
    | Ok res -> parse res
    | _ -> failwith "Error tokenizing."

[<Tests>]
let tests =
    testList
        "Parser Test"
        [ testCase "Parse empty script" <| fun _ -> Expect.equal (parse "") (Ok []) ""
          testCase "read empty quote"
          <| fun _ -> Expect.equal (parse "[]") (Ok [ Any.Quote [] ]) ""
          testCase "read quote"
          <| fun _ ->
              Expect.equal
                  (parse "[test \"test2\"]")
                  (Ok [ Any.Quote [ Any.Term(Term "test"); Any.Literal(Literal "test2") ] ])
                  ""
          testCase "read empty record"
          <| fun _ -> Expect.equal (parse "{}") (Ok [ Any.Record Map.empty ]) ""
          testCase "read basic block"
          <| fun _ -> Expect.equal (parse "(2)") (Ok [ Any.Application(Term "2", []) ]) "" ]
