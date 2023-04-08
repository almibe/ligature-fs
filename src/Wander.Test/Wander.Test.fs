// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Main.Test

open Expecto
open Ligature
open Ligature.Wander.Model
open Ligature.Wander.Main

let ident id =
    Identifier(
        match identifier id with
        | Ok(v) -> v
        | Error(_) -> todo
    )

[<Tests>]
let tests =
    testList
        "Main Test"
        [ testCase "Run Integer"
          <| fun _ ->
              let script = "1235"
              let result = run script
              Expect.equal result (Ok(Integer(1235))) ""
          testCase "Run String"
          <| fun _ ->
              let script = "\"Hello\""
              let result = run script
              Expect.equal result (Ok(String("Hello"))) ""
          testCase "Run Booleans"
          <| fun _ ->
              let script = "true"
              let result = run script
              Expect.equal result (Ok(Boolean(true))) ""
              let script = "false"
              let result = run script
              Expect.equal result (Ok(Boolean(false))) ""
          testCase "Run Identifier"
          <| fun _ ->
              let script = "<hello>"
              let result = run script
              Expect.equal result (Ok(ident "hello")) ""
          testCase "Handle WhiteSpace"
          <| fun _ ->
              let script = "  \n  5   "
              let result = run script
              Expect.equal result (Ok(Integer(5))) ""
          testCase "Handle Multiple Values and White Space"
          <| fun _ ->
              let script = " 1  true  \n  \"hello\" \r\n 5  321 \n"
              let result = run script
              Expect.equal result (Ok(Integer(321))) ""
          testCase "Let Statement"
          <| fun _ ->
              let script = "let x = 5"
              let result = run script
              Expect.equal result (Ok(Nothing)) ""
          testCase "Let Statement with Value Reference"
          <| fun _ ->
              let script = "let x = 5\nx"
              let result = run script
              Expect.equal result (Ok(Integer(5))) ""
          testCase "If Expression"
          <| fun _ ->
              let script = "if true false else true"
              let result = run script
              Expect.equal result (Ok(Boolean(false))) ""
          testCase "Calling Native Function"
          <| fun _ ->
              let script = "not(true)"
              let result = run script
              Expect.equal result (Ok(Boolean(false))) ""
          testCase "Nesting Native Function Calls"
          <| fun _ ->
              let script = "not(not(true))"
              let result = run script
              Expect.equal result (Ok(Boolean(true))) "" ]
