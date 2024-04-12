// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lexer.Test

open Expecto
open Ligature.Wander.Lexer
open Ligature

let ident id =
    Token.Identifier(
        match identifier id with
        | Ok(v) -> v
        | Error(_) -> failwith "todo"
    )

[<Tests>]
let tests =
    testList
        "Lexer Test"
        [ testCase "Allow empty input" <| fun _ -> Expect.equal (tokenize "") (Ok []) ""
          testCase "Read Integer Token"
          <| fun _ ->
              Expect.equal (tokenize "123") (Ok([ Token.Int(123) ])) ""
              Expect.equal (tokenize "0") (Ok([ Token.Int(0) ])) ""
              Expect.equal (tokenize "-4123") (Ok([ Token.Int(-4123) ])) ""
          testCase "Read Bytes Token"
          <| fun _ ->
              Expect.equal (tokenize "0x00") (Ok([ Token.Bytes([| 0x00uy |]) ])) ""
              Expect.equal (tokenize "0x01") (Ok([ Token.Bytes([| 0x01uy |]) ])) ""
              Expect.equal (tokenize "0xFF") (Ok([ Token.Bytes([| 0xffuy |]) ])) ""
          testCase "Read Names"
          <| fun _ ->
              Expect.equal (tokenize "x") (Ok([ Token.Name("x") ])) ""
              Expect.equal (tokenize "hello") (Ok([ Token.Name("hello") ])) ""
          testCase "tokenize booleans"
          <| fun _ ->
              Expect.equal (tokenize "true") (Ok([ Token.Bool(true) ])) ""
              Expect.equal (tokenize "false") (Ok([ Token.Bool(false) ])) ""
          testCase "tokenize whitespace"
          <| fun _ ->
              Expect.equal (tokenize " ") (Ok([ Token.WhiteSpace(" ") ])) ""
              Expect.equal (tokenize "   ") (Ok([ Token.WhiteSpace("   ") ])) ""
          testCase "tokenize strings"
          <| fun _ ->
              Expect.equal (tokenize "\"hello\"") (Ok([ Token.StringLiteral("hello") ])) ""
              Expect.equal (tokenize "\"\"") (Ok([ Token.StringLiteral("") ])) ""
          //Expect.equal (tokenize "\t") (Ok([WhiteSpace("\t")])) ""
          //Expect.equal (tokenize "\t  ") (Ok([WhiteSpace("\t  ")])) ""
          testCase "tokenize new lines"
          <| fun _ ->
              Expect.equal (tokenize "\n") (Ok([ Token.NewLine("\n") ])) ""
              Expect.equal (tokenize "\r\n") (Ok([ Token.NewLine("\r\n") ])) ""
              Expect.equal (tokenize "\r\n\r\n\r\n\n") (Ok([ Token.NewLine("\r\n\r\n\r\n\n") ])) ""
          testCase "Read Identifiers"
          <| fun _ ->
              Expect.equal (tokenize "`a`") (Ok([ (ident "a") ])) ""
              Expect.equal (tokenize "`https://ligature.dev/#`") (Ok([ ident "https://ligature.dev/#" ])) ""
          testCase "Read comments"
          <| fun _ ->
              Expect.equal (tokenize "--") (Ok([ Token.Comment("--") ])) ""
              Expect.equal (tokenize "--hello") (Ok([ Token.Comment("--hello") ])) ""

              Expect.equal
                  (tokenize "-- this is a@#$@%$#@$%@ comment;;;;  ")
                  (Ok([ Token.Comment("-- this is a@#$@%$#@$%@ comment;;;;  ") ]))
                  ""
          testCase "read String Literal"
          <| fun _ -> Expect.equal (tokenize @"""hello""") (Ok([ Token.StringLiteral("hello") ])) ""
          // testCase "read Bytes Literal" <| fun _ ->
          //     Expect.equal (tokenize "0x55") (Ok([Bytes("0x55")])) ""
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

              Expect.equal
                  (tokenize "{ true }")
                  (Ok(
                      [ Token.OpenBrace
                        Token.WhiteSpace(" ")
                        Token.Bool(true)
                        Token.WhiteSpace(" ")
                        Token.CloseBrace ]
                  ))
                  ""
          testCase "read colon"
          <| fun _ ->
              Expect.equal (tokenize ":") (Ok([ Token.Colon ])) ""
              Expect.equal (tokenize "::::") (Ok([ Token.Colon; Token.Colon; Token.Colon; Token.Colon ])) ""
          testCase "read dot"
          <| fun _ ->
              Expect.equal (tokenize ".") (Ok([ Token.Dot ])) ""
              Expect.equal (tokenize "....") (Ok([ Token.Dot; Token.Dot; Token.Dot; Token.Dot ])) ""
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
          testCase "read arrows and hash"
          <| fun _ ->
              Expect.equal (tokenize "->") (Ok([ Token.Arrow ])) ""
              Expect.equal (tokenize "=>") (Ok([ Token.WideArrow ])) ""
              Expect.equal (tokenize "#") (Ok([ Token.Hash ])) ""
              Expect.equal (tokenize "->->") (Ok([ Token.Arrow; Token.Arrow ])) ""
              Expect.equal (tokenize "->->->") (Ok([ Token.Arrow; Token.Arrow; Token.Arrow ])) ""
          testCase "read when keyword"
          <| fun _ -> Expect.equal (tokenize "when") (Ok([ Token.WhenKeyword ])) ""
          testCase "read question mark"
          <| fun _ -> Expect.equal (tokenize "?") (Ok([ Token.QuestionMark ])) ""
          testCase "read simple let expression"
          <| fun _ ->
              let ws = Token.WhiteSpace(" ")
              Expect.equal (tokenize "x = 5") (Ok([ Token.Name("x"); ws; Token.EqualsSign; ws; Token.Int(5) ])) ""
          testCase "return error on invalid input"
          <| fun _ -> Expect.isError (tokenize "\"") ""
          testCase "read pipe expression"
          <| fun _ -> Expect.equal (tokenize "|") (Ok([ Token.Pipe ])) "" ]
