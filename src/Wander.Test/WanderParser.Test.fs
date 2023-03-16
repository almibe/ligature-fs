// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Parser.Test

open Expecto
open Ligature
open Wander.Parser
open Wander.Lexer
open Wander.Model

let unsafe result =
    match result with
    | Ok(v) -> v
    | Error(_) -> todo

let ident id = Identifier(unsafe (identifier id))

[<Tests>]
let tests =
    testList
        "Parser Suite"
        [ testCase "Parse Integer"
          <| fun _ ->
              let tokens = [ WanderToken.Integer(345) ]
              let ast = parse tokens
              Expect.equal ast (Ok([ Value(Integer(345)) ])) ""
          testCase "Parse Let Statements"
          <| fun _ ->
              let tokens = Wander.Lexer.tokenize "let x = 5"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ LetStatement("x", Value(Integer(5))) ])) ""
          testCase "Parse Let Statement with Name"
          <| fun _ ->
              let tokens = Wander.Lexer.tokenize "let x = 5\nx"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ LetStatement("x", Value(Integer(5))); Name("x") ])) ""
          // testCase "Parse Integer" <| fun _ ->
          //     Expect.equal (parse "123") (Ok([Value(Integer(123))])) ""
          //     Expect.equal (parse "0") (Ok([Value(Integer(0))])) ""
          //     Expect.equal (parse "-4123") (Ok([Value(Integer(-4123))])) ""
          // testCase "Read Names" <| fun _ ->
          //     Expect.equal (parse "hello") (Ok([Name("hello")])) ""
          // testCase "parse booleans" <| fun _ ->
          //     Expect.equal (parse "true") (Ok([Value(Boolean(true))])) ""
          //     Expect.equal (parse "false")  (Ok([Value(Boolean(false))])) ""
          // ftestCase "parse whitespace" <| fun _ ->
          //     Expect.equal (parse " ") (Ok([])) ""
          //     Expect.equal (parse "   ") (Ok([])) ""
          //     //Expect.equal (parse "\t") (Ok([WhiteSpace("\t")])) ""
          //     //Expect.equal (parse "\t  ") (Ok([WhiteSpace("\t  ")])) ""
          // testCase "parse new lines" <| fun _ ->
          //     Expect.equal (parse "\n") (Ok([])) ""
          //     Expect.equal (parse "\r\n") (Ok([])) ""
          //     Expect.equal (parse "\r\n\r\n\r\n\n") (Ok([])) ""
          // testCase "Read Identifiers" <| fun _ ->
          //     Expect.equal (parse "<a>") (Ok([Value(ident "a")])) ""
          //     Expect.equal (parse "<https://ligature.dev/#>") (Ok([Value(ident "https://ligature.dev/#")])) ""
          // testCase "Read comments" <| fun _ ->
          //     Expect.equal (parse "--") (Ok([])) ""
          //     Expect.equal (parse "--hello") (Ok([])) ""
          //     Expect.equal (parse "-- this is a@#$@%$#@$%@ comment;;;;  ") (Ok([])) ""
          //     Expect.equal (parse "-- this is \n--  a@#$@%$#@$%@ comment;;;;  ") (Ok([])) ""
          // testCase "read String Literal" <| fun _ ->
          //     Expect.equal (parse @"""hello""") (Ok([Value(String("hello"))])) ""
          // // testCase "read Bytes Literal" <| fun _ ->
          // //     Expect.equal (parse "0x55") (Ok([Bytes("0x55")])) ""
          // testCase "read let statement" <| fun _ ->
          //     Expect.equal (parse "let x = 6") (Ok([LetStatement("x", Value(Integer(6)))])) ""
          //     Expect.equal (parse "let x=8") (Ok([LetStatement("x", Value(Integer(6)))])) ""
          //     Expect.equal (parse "let x = \n true") (Ok([LetStatement("x", Value(Boolean(true)))])) ""
          // testCase "read let with scope" <| fun _ ->
          //     Expect.equal (parse "let x = { true }") (Ok([todo])) ""
          //     Expect.equal (parse "{ let x = 6 }") (Ok([todo])) ""
          //     Expect.equal (parse "{ let x = { false } }") (Ok([todo])) ""
          // // testCase "read colon" <| fun _ ->
          // //     Expect.equal (parse ":") (Ok([Colon])) ""
          // //     Expect.equal (parse "::::") (Ok([Colon; Colon; Colon; Colon])) ""
          // // testCase "read dot" <| fun _ ->
          // //     Expect.equal (parse ".") (Ok([Dot])) ""
          // //     Expect.equal (parse "....") (Ok([Dot; Dot; Dot; Dot])) ""
          // // testCase "read parens" <| fun _ ->
          // //     Expect.equal (parse "(") (Ok([OpenParen])) ""
          // //     Expect.equal (parse ")") (Ok([CloseParen])) ""
          // //     Expect.equal (parse "(()))") (Ok([OpenParen; OpenParen; CloseParen; CloseParen; CloseParen])) ""
          // // testCase "read square brackets" <| fun _ ->
          // //     Expect.equal (parse "[") (Ok([OpenSquare])) ""
          // //     Expect.equal (parse "]") (Ok([CloseSquare])) ""
          // //     Expect.equal (parse "[[]]]") (Ok([OpenSquare; OpenSquare; CloseSquare; CloseSquare; CloseSquare])) ""
          // // testCase "read arrow" <| fun _ ->
          // //     Expect.equal (parse "->") (Ok([Arrow])) ""
          // //     Expect.equal (parse "->->") (Ok([Arrow; Arrow])) ""
          // //     Expect.equal (parse "->->->") (Ok([Arrow; Arrow; Arrow])) ""
          // // testCase "read if and else keywords" <| fun _ ->
          // //     Expect.equal (parse "if") (Ok([IfKeyword])) ""
          // //     Expect.equal (parse "elsif") (Ok([ElsifKeyword])) ""
          // //     Expect.equal (parse "else") (Ok([ElseKeyword])) ""
          // // testCase "read question mark" <| fun _ ->
          // //     Expect.equal (parse "?") (Ok([QuestionMark])) ""
          ]
