// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Main.Test

open Expecto
open Ligature.Wander.Model
open Ligature.Wander.Main
open Ligature.Wander.Identifier

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

let ident id =
    WanderValue.Identifier(
        match identifier id with
        | Ok v -> v
        | Error _ -> todo
    )

let bindings = Ligature.Wander.Preludes.standardPrelude ()

[<Tests>]
let tests =
    testList
        "Main Test"
        [ testCase "Run Integer"
          <| fun _ ->
              let script = "1235"
              let result = run script bindings
              Expect.equal result (Ok(WanderValue.Int(1235))) ""
          testCase "Run String"
          <| fun _ ->
              let script = "\"Hello\""
              let result = run script bindings
              Expect.equal result (Ok(WanderValue.String("Hello"))) ""
          testCase "Run Booleans"
          <| fun _ ->
              let script = "true"
              let result = run script bindings
              Expect.equal result (Ok(WanderValue.Bool(true))) ""
              let script = "false"
              let result = run script bindings
              Expect.equal result (Ok(WanderValue.Bool(false))) ""
          testCase "Run Identifier"
          <| fun _ ->
              let script = "<hello>"
              let result = run script bindings
              Expect.equal result (Ok(ident "hello")) ""
          testCase "Handle WhiteSpace"
          <| fun _ ->
              let script = "  \n  5   "
              let result = run script bindings
              Expect.equal result (Ok(WanderValue.Int(5))) ""
          testCase "Handle Multiple Values and White Space"
          <| fun _ ->
              let script = " 1,  true,  \n  \"hello\", \r\n 5,  321 \n"
              let result = run script bindings
              Expect.equal result (Ok(WanderValue.Int(321))) ""
          testCase "Let Statement"
          <| fun _ ->
              let script = "let x 5"
              let result = run script bindings
              Expect.equal result (Ok(WanderValue.Int(5))) ""
          testCase "Let Statement with Value Reference"
          <| fun _ ->
              let script = "let x 5,\nx"
              let result = run script bindings
              Expect.equal result (Ok(WanderValue.Int(5))) ""
          testCase "Define and call Lambda"
          <| fun _ ->
              let script = "let id \\x -> x,\nid 45"
              let result = run script bindings
              Expect.equal result (Ok(WanderValue.Int(45))) ""
        //   testCase "Let Statement with Value Reference In Scope"
        //   <| fun _ ->
        //       let script = "(let x 5, x)"
        //       let result = run script bindings
        //       Expect.equal result (Ok(WanderValue.Int(5))) ""
        //   testCase "Let Statement with Value Reference Outside Scope"
        //   <| fun _ ->
        //       let script = "let x = 4 { let x = 5\nx } x"
        //       let result = run script bindings
        //       Expect.equal result (Ok(WanderValue.Integer(4))) ""
        //   testCase "If Expression"
        //   <| fun _ ->
        //       let script = "if true false else true"
        //       let result = run script bindings
        //       Expect.equal result (Ok(WanderValue.Boolean(false))) ""
        //   testCase "Testing Scopes with If Expressions"
        //   <| fun _ ->
        //       let script = "let x = 5 if { let x = 4 true} x else true"
        //       let result = run script bindings
        //       Expect.equal result (Ok(WanderValue.Integer(5))) ""
        //   testCase "Calling Native Function"
        //   <| fun _ ->
        //       let script = "not(true)"
        //       let result = run script bindings
        //       Expect.equal result (Ok(WanderValue.Boolean(false))) ""
        //   testCase "Nesting Native Function Calls"
        //   <| fun _ ->
        //       let script = "not(not(true))"
        //       let result = run script bindings
        //       Expect.equal result (Ok(WanderValue.Boolean(true))) ""
        //   testCase "Define and call lambda"
        //   <| fun _ ->
        //       let script = "let x = { -> true}\nx()"
        //       let result = run script bindings
        //       Expect.equal result (Ok(WanderValue.Boolean(true))) ""
        //   testCase "Define and call lambda with parameter"
        //   <| fun _ ->
        //       let script = "let id = { x -> x }\nid(true)"
        //       let result = run script bindings
        //       Expect.equal result (Ok(WanderValue.Boolean(true))) ""
        //   testCase "Define and call lambda with parameter and native function calls"
        //   <| fun _ ->
        //       let script = "let same = { x -> not(not(x)) }\nlet y = true\nsame(y)"
        //       let result = run script bindings
        //       Expect.equal result (Ok(WanderValue.Boolean(true))) "" 
        //   testCase "Basic Tuple Tests"
        //   <| fun _ ->
        //       Expect.equal (run "()" bindings) (Ok(WanderValue.Tuple([]))) "Empty tuple"
        //       Expect.equal (run "(true false 1 2 \"hello\")" bindings) (Ok(WanderValue.Tuple([WanderValue.Boolean(true); WanderValue.Boolean(false); WanderValue.Integer(1); WanderValue.Integer(2); WanderValue.String("hello")]))) "Tuple with Values"
        //       Expect.equal (run "( if true false else true )" bindings) (Ok(WanderValue.Tuple([WanderValue.Boolean(false)]))) "Tuple with Conditional"
        //       Expect.equal (run "( { let x = 5 x} 5 6)" bindings) (Ok(WanderValue.Tuple([WanderValue.Integer(5); WanderValue.Integer(5); WanderValue.Integer(6)]))) ""
        //       //Expect.equal (run "( { x -> x }(5) )") (Ok(Tuple([Integer(5)]))) ""
        ]
