// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Tokenizer.Test

open Expecto
open Wander.Tokenizer
open Wander.Model
open Wander.Main
open Ligature.Main

[<Tests>]
let tests =
    testList
        "Tokenizer Test"
        [ testCase "Allow empty input" <| fun _ -> Expect.equal (tokenize "") (Ok []) ""
          //   testCase "Read Integer Token"
          //   <| fun _ ->
          //   Expect.equal (tokenize "123") (Ok([ Token.Int(123I) ])) ""
          //   Expect.equal (tokenize "0") (Ok([ Token.Int(0I) ])) ""
          //   Expect.equal (tokenize "-4123") (Ok([ Token.Int(-4123I) ])) ""
          //   testCase "Read Bytes Token"
          //   <| fun _ ->
          //       Expect.equal (tokenize "0x00") (Ok([ Token.Bytes([| 0x00uy |]) ])) ""
          //       Expect.equal (tokenize "0x01") (Ok([ Token.Bytes([| 0x01uy |]) ])) ""
          //       Expect.equal (tokenize "0xFF") (Ok([ Token.Bytes([| 0xffuy |]) ])) ""
          testCase "Read Symbols"
          <| fun _ ->
              Expect.equal (tokenize "a-x") (Ok([ Token.Symbol(Symbol("a-x")) ])) ""
              Expect.equal (tokenize "x") (Ok([ Token.Symbol(Symbol("x")) ])) ""
              Expect.equal (tokenize "hello") (Ok([ Token.Symbol(Symbol "hello") ])) ""
              Expect.equal (tokenize "x.y") (Ok([ Token.Symbol(Symbol "x.y") ])) ""
              Expect.equal (tokenize "hello.t.x.a") (Ok([ Token.Symbol(Symbol "hello.t.x.a") ])) ""
              Expect.equal (tokenize "$") (Ok([ Token.Symbol(Symbol("$")) ])) ""
              Expect.equal (tokenize "$a") (Ok([ Token.Symbol(Symbol("$a")) ])) ""
              Expect.equal (tokenize "$this_is_also234") (Ok([ Token.Symbol(Symbol("$this_is_also234")) ])) ""
              Expect.equal (tokenize "0") (Ok([ Token.Symbol(Symbol("0")) ])) ""
              Expect.equal (tokenize "-100") (Ok([ Token.Symbol(Symbol("-100")) ])) ""
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
              Expect.equal (tokenize "\r\n") (Ok([ Token.NewLine("\r\n") ])) ""
              Expect.equal (tokenize "\r\n\r\n\r\n\n") (Ok([ Token.NewLine("\r\n\r\n\r\n\n") ])) ""
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
          testCase "read question mark"
          <| fun _ -> Expect.equal (tokenize "?") (Ok([ Token.Symbol(Symbol "?") ])) ""
          testCase "return error on invalid input"
          <| fun _ -> Expect.isError (tokenize "\"") "" ]
