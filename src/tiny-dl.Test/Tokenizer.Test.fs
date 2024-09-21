// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Tokenizer.Test

open Expecto
open TinyDL.Tokenizer

[<Tests>]
let tests =
    testList
        "Lexer Tests"
        [ testCase "Tokenize empty script"
          <| fun _ -> Expect.equal (tokenize "") (Ok []) ""
          testCase "Tokenize Names"
          <| fun _ ->
              Expect.equal
                  (tokenize "name another-name")
                  (Ok [ Token.Name "name"; Token.WhiteSpace " "; Token.Name "another-name" ])
                  ""
          testCase "Tokenize symbols"
          <| fun _ ->
              Expect.equal
                  (tokenize ":,.∃∀⊑⊔≡⊤¬⊥⊓{}()[]")
                  (Ok
                      [ Token.Colon
                        Token.Comma
                        Token.Dot
                        Token.Exists
                        Token.All
                        Token.ConceptInclusion
                        Token.ConceptDisjunction
                        Token.Definition
                        Token.Top
                        Token.Negation
                        Token.Bottom
                        Token.ConceptConjunction
                        Token.OpenBrace
                        Token.CloseBrace
                        Token.OpenParen
                        Token.CloseParen
                        Token.OpenSquare
                        Token.CloseSquare ])
                  ""
          testCase "Tokenize Uniary Predicate with Conjunction"
          <| fun _ ->
              Expect.equal
                  (tokenize "x:Y⊓Z")
                  (Ok
                      [ Token.Name "x"
                        Token.Colon
                        Token.Name "Y"
                        Token.ConceptConjunction
                        Token.Name "Z" ])
                  "" ]
