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
          testCase "read call with network passed"
          <| fun _ ->
              Expect.equal
                  (parse "a {a b c}")
                  (Ok(
                      [ Expression.Call(
                            Element "a",
                            [ Any.Network(
                                  Set.ofList
                                      [ (ElementPattern.Element(Element "a"),
                                         ElementPattern.Element(Element "b"),
                                         Value.Element(Element "c")) ]
                              ) ]
                        ) ]
                  ))
                  ""
          testCase "read network with attribute"
          <| fun _ ->
              Expect.equal
                  (parse "a {a b \"c\"}")
                  (Ok(
                      [ Expression.Call(
                            Element "a",
                            [ Any.Network(
                                  Set.ofList
                                      [ ElementPattern.Element(Element "a"),
                                        ElementPattern.Element(Element "b"),
                                        Value.Literal "c" ]
                              ) ]
                        ) ]
                  ))
                  ""
          testCase "read network with quote in value"
          <| fun _ ->
              Expect.equal
                  (parse "a {a b ()}")
                  (Ok(
                      [ Expression.Call(
                            Element "a",
                            [ Any.Network(
                                  Set.ofList
                                      [ ElementPattern.Element(Element "a"),
                                        ElementPattern.Element(Element "b"),
                                        Value.Quote [] ]
                              ) ]
                        ) ]
                  ))
                  ""
          testCase "read call with pattern passed"
          <| fun _ ->
              Expect.equal
                  (parse "a {?a b c}")
                  (Ok(
                      [ Expression.Call(
                            Element "a",
                            [ Any.Network(
                                  Set.ofList
                                      [ ElementPattern.Variable(Variable "?a"),
                                        ElementPattern.Element(Element "b"),
                                        Value.Element(Element "c") ]
                              ) ]
                        ) ]
                  ))
                  ""
          testCase "read call with pipe"
          <| fun _ ->
              Expect.equal
                  (parse "id {a b c} | count")
                  (Ok(
                      [ Expression.Call(
                            Element "count",
                            [ Any.Quote(
                                  [ Any.Element(Element "id")
                                    Any.Network(
                                        Set.ofList
                                            [ ElementPattern.Element(Element "a"),
                                              ElementPattern.Element(Element "b"),
                                              Value.Element(Element "c") ]
                                    ) ]
                              ) ]
                        ) ]
                  ))
                  "" ]
