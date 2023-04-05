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
          testCase "Parse Integers" <| fun _ ->
              Expect.equal (parseString "123") (Ok([Value(Integer(123))])) ""
              Expect.equal (parseString "0") (Ok([Value(Integer(0))])) ""
              Expect.equal (parseString "-4123") (Ok([Value(Integer(-4123))])) ""
          testCase "Read Names" <| fun _ ->
              Expect.equal (parseString "hello") (Ok([Name("hello")])) ""
          testCase "parse booleans" <| fun _ ->
              Expect.equal (parseString "true") (Ok([Value(Boolean(true))])) ""
              Expect.equal (parseString "false") (Ok([Value(Boolean(false))])) ""
              Expect.equal (parseString "true false    true \n false false") (Ok([
                Value(Boolean(true))
                Value(Boolean(false))
                Value(Boolean(true))
                Value(Boolean(false))
                Value(Boolean(false))
              ])) ""
          testCase "parse whitespace" <| fun _ ->
              Expect.equal (parseString " ") (Ok([])) ""
              Expect.equal (parseString "   ") (Ok([])) ""
              //Expect.equal (parse "\t") (Ok([WhiteSpace("\t")])) ""
              //Expect.equal (parse "\t  ") (Ok([WhiteSpace("\t  ")])) ""
          testCase "parse new lines" <| fun _ ->
              Expect.equal (parseString "\n") (Ok([])) ""
              Expect.equal (parseString "\r\n") (Ok([])) ""
              Expect.equal (parseString "\r\n\r\n\r\n\n") (Ok([])) ""
          testCase "Read Identifiers" <| fun _ ->
              Expect.equal (parseString "<a>") (Ok([Value(ident "a")])) ""
              Expect.equal (parseString "<https://ligature.dev/#>") (Ok([Value(ident "https://ligature.dev/#")])) ""
          testCase "Read comments" <| fun _ ->
              Expect.equal (parseString "--") (Ok([])) ""
              Expect.equal (parseString "--hello") (Ok([])) ""
              Expect.equal (parseString "-- this is a@#$@%$#@$%@ comment;;;;  ") (Ok([])) ""
              Expect.equal (parseString "-- this is \n--  a@#$@%$#@$%@ comment;;;;  ") (Ok([])) ""
          testCase "read String Literal" <| fun _ ->
              Expect.equal (parseString @"""hello""") (Ok([Value(String("hello"))])) ""
          // // testCase "read Bytes Literal" <| fun _ ->
          // //     Expect.equal (parse "0x55") (Ok([Bytes("0x55")])) ""
          testCase "read let statement" <| fun _ ->
              Expect.equal (parseString "let x = 6") (Ok([LetStatement("x", Value(Integer(6)))])) ""
              Expect.equal (parseString "let x=8") (Ok([LetStatement("x", Value(Integer(8)))])) ""
              Expect.equal (parseString "let x = \n true") (Ok([LetStatement("x", Value(Boolean(true)))])) ""
              Expect.equal (parseString @"let x = ""true""") (Ok([LetStatement("x", Value(String("true")))])) ""
              Expect.equal (parseString "let x = <a>") (Ok([LetStatement("x", Value(ident "a"))])) ""
          testCase "read Scopes" <| fun _ ->
              Expect.equal (parseString "{ true }") (Ok [Scope [Value(Boolean(true))] ]) ""
              Expect.equal (parseString "{ 55 }") (Ok [Scope [Value(Integer(55))] ]) ""
              Expect.equal (parseString @"{ 1 true 3 ""Hello""}") (Ok [Scope [Value(Integer(1)); Value(Boolean(true)); Value(Integer(3)); Value(String("Hello")) ]]) ""
          testCase "read let with scope" <| fun _ ->
              Expect.equal (parseString "let x = { true }") (Ok([LetStatement("x", Scope([Value(Boolean(true))]))])) ""
              Expect.equal (parseString "{ let x = 6 }") (Ok([Scope([LetStatement("x", Value(Integer(6)))])])) ""
              Expect.equal (parseString "{ let x = { false } }") (Ok([Scope([LetStatement("x", Scope([Value(Boolean(false))]))])])) ""
        //TODO parsing conditionals
        //TODO parsing function calls
        //TODO parsing lambdas
          ]
