// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Tokenizer.Test

open Expecto
open Wander.Tokenizer

[<Tests>]
let tests =
    testList
        "Tokenizer Test"
        [ testCase "Allow empty input" <| fun _ -> Expect.equal (tokenize "") (Ok []) ""
          testCase "Read Elements"
          <| fun _ ->
              Expect.equal (tokenize "a-x") (Ok([ Token.Element("a-x") ])) ""
              Expect.equal (tokenize "x") (Ok([ Token.Element("x") ])) ""
              Expect.equal (tokenize "hello") (Ok([ Token.Element("hello") ])) ""
              Expect.equal (tokenize "x.y") (Ok([ Token.Element("x.y") ])) ""
              Expect.equal (tokenize "hello.t.x.a") (Ok([ Token.Element("hello.t.x.a") ])) ""
              Expect.equal (tokenize "$") (Ok([ Token.Element("$") ])) ""
              Expect.equal (tokenize "$a") (Ok([ Token.Element("$a") ])) ""
              Expect.equal (tokenize "$this_is_also234") (Ok([ Token.Element("$this_is_also234") ])) ""
              Expect.equal (tokenize "0") (Ok([ Token.Element("0") ])) ""
              Expect.equal (tokenize "?_") (Ok([ Token.Variable("?_") ])) ""
              Expect.equal (tokenize "?test") (Ok([ Token.Variable "?test" ])) ""
              Expect.equal (tokenize "-100") (Ok([ Token.Element("-100") ])) ""
          testCase "tokenize whitespace"
          <| fun _ ->
              Expect.equal (tokenize " ") (Ok([ Token.WhiteSpace(" ") ])) ""
              Expect.equal (tokenize "   ") (Ok([ Token.WhiteSpace("   ") ])) ""
          //Expect.equal (tokenize "\t") (Ok([Token.WhiteSpace("\t")])) ""
          //Expect.equal (tokenize "\t  ") (Ok([Token.WhiteSpace("\t  ")])) ""
          testCase "tokenize strings"
          <| fun _ ->
              Expect.equal (tokenize "\"hello\"") (Ok([ Token.StringLiteral("hello") ])) ""
              Expect.equal (tokenize "\"\"") (Ok([ Token.StringLiteral("") ])) ""
          testCase "tokenize new lines"
          <| fun _ ->
              Expect.equal (tokenize "\n") (Ok([ Token.NewLine("\n") ])) ""
              Expect.equal (tokenize "\r\n") (Ok([ Token.NewLine("\n") ])) ""
              Expect.equal (tokenize "\r\n\r\n\r\n\n") (Ok([ Token.NewLine("\n\n\n\n") ])) ""
          //   testCase "Read comments"
          //   <| fun _ ->
          //       Expect.equal (tokenize "--") (Ok([ Token.Comment("--") ])) ""
          //       Expect.equal (tokenize "--hello") (Ok([ Token.Comment("--hello") ])) ""

          //       Expect.equal
          //           (tokenize "-- this is a@#$@%$#@$%@ comment;;;;  ")
          //           (Ok([ Token.Comment("-- this is a@#$@%$#@$%@ comment;;;;  ") ]))
          //           ""
          testCase "read String Literal"
          <| fun _ -> Expect.equal (tokenize @"""hello""") (Ok([ Token.StringLiteral("hello") ])) ""
          testCase "read braces"
          <| fun _ ->
              Expect.equal (tokenize "{") (Ok([ Token.OpenBrace ])) ""
              Expect.equal (tokenize "}") (Ok([ Token.CloseBrace ])) ""

              Expect.equal
                  (tokenize "{{}}}")
                  (Ok(
                      [ Token.OpenBrace
                        Token.OpenBrace
                        Token.CloseBrace
                        Token.CloseBrace
                        Token.CloseBrace ]
                  ))
                  ""

          testCase "read parens"
          <| fun _ ->
              Expect.equal (tokenize "(") (Ok([ Token.OpenParen ])) ""
              Expect.equal (tokenize ")") (Ok([ Token.CloseParen ])) ""

              Expect.equal
                  (tokenize "(()))")
                  (Ok(
                      [ Token.OpenParen
                        Token.OpenParen
                        Token.CloseParen
                        Token.CloseParen
                        Token.CloseParen ]
                  ))
                  ""
          testCase "read square brackets"
          <| fun _ ->
              Expect.equal (tokenize "[") (Ok([ Token.OpenSquare ])) ""
              Expect.equal (tokenize "]") (Ok([ Token.CloseSquare ])) ""

              Expect.equal
                  (tokenize "[[]]]")
                  (Ok(
                      [ Token.OpenSquare
                        Token.OpenSquare
                        Token.CloseSquare
                        Token.CloseSquare
                        Token.CloseSquare ]
                  ))
                  ""
          testCase "read variable"
          <| fun _ -> Expect.equal (tokenize "?") (Ok([ Token.Variable("?") ])) ""
          testCase "read named variable"
          <| fun _ -> Expect.equal (tokenize "?test") (Ok([ Token.Variable("?test") ])) ""
          testCase "return error on invalid input"
          <| fun _ -> Expect.isError (tokenize "\"") "" ]
