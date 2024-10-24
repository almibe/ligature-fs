// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.NewMain.Test

open Expecto
open Wander.Main
open Ligature.Main
open Wander.Tokenizer
open Wander.Parser
open Ligature.InMemoryStore
open Wander.Model

[<Tests>]
let tests =
    testList
        "New Main Test"
        [ testCase "Empty script"
          <| fun _ ->
              let script = ""
              let result = run Map.empty (emptyInMemoryStore ()) script
              Expect.equal result (Ok None) ""
            testCase "Run Empty Network"
            <| fun _ ->
                let script = "id {}"
                let result = run Map.empty (emptyInMemoryStore ()) script
                Expect.equal result (Ok None) ""

            // testCase "Parse Nested Expressions"
            // <| fun _ ->
            //     let script = "(a (b (c) ))"

            //     match tokenize script with
            //     | Ok(res) ->
            //         match parse res with
            //         | Ok(res) ->
            //             Expect.equal
            //                 res
            //                 [ ParserElement.Expression
            //                       [ ParserElement.Name("a")
            //                         ParserElement.Expression
            //                             [ ParserElement.Name("b")
            //                               ParserElement.Expression [ ParserElement.Name("c") ] ] ] ]
            //                 ""

            //             let element = express res []

            //             Expect.equal
            //                 element
            //                 [ Element.Expression
            //                       [ Identifier.Name(Name("a"))
            //                         Identifier.Expression
            //                             [ Identifier.Name(Name("b"))
            //                               Identifier.Expression [ Identifier.Name(Name("c")) ] ] ] ]
            //                 ""
            //         | Error err -> failwith $"Error Parsing - {err.UserMessage}"
            //     | _ -> failwith "Error"

          testCase "Parse Expression Call"
          <| fun _ ->
              let script = "assert-equal true true"

              match tokenize script with
              | Ok(res) -> Expect.isOk (parse res) ""
              | Error err -> failwith $"Error Parsing - {err.UserMessage}"
              | _ -> failwith "Error"
          ]
