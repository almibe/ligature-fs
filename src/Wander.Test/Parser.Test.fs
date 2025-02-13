// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Parser.Test

open Expecto
open Wander.Tokenizer
open Ligature.Model

let parse input =
    match tokenize input with
    | Ok res -> parse res
    | _ -> failwith "Error tokenizing."

[<Tests>]
let tests =
    testList
        "Parser Test"
        [ testCase "Parse empty script" <| fun _ -> Expect.equal (parse "") (Ok []) ""
          testCase "read call with empty network passed"
          <| fun _ -> Expect.equal (parse "{}") (Ok([ Any.Network Set.empty ])) ""
          testCase "read call with single count network passed"
          <| fun _ ->
              Expect.equal
                  (parse "{a b c}")
                  (Ok(
                      [ Any.Network(
                            Set.ofList
                                [ (ElementPattern.Element(Element "a"),
                                   ElementPattern.Element(Element "b"),
                                   ElementPattern.Element(Element "c")) ]
                        ) ]
                  ))
                  ""
          testCase "read network with attribute"
          <| fun _ ->
              Expect.equal
                  (parse "{a b \"c\"}")
                  (Ok(
                      [ Any.Network(
                            Set.ofList
                                [ ElementPattern.Element(Element "a"),
                                  ElementPattern.Element(Element "b"),
                                  ElementPattern.Element(Element "c") ]
                        ) ]

                  ))
                  ""
          testCase "read empty quote"
          <| fun _ -> Expect.equal (parse "[]") (Ok([ Any.Quote [] ])) ""
          testCase "read call with pattern passed"
          <| fun _ ->
              Expect.equal
                  (parse "{?a b c}")
                  (Ok(
                      [ Any.Network(
                            Set.ofList
                                [ ElementPattern.Variable(Variable "?a"),
                                  ElementPattern.Element(Element "b"),
                                  ElementPattern.Element(Element "c") ]
                        ) ]
                  ))
                  ""
          testCase "read multiple network script"
          <| fun _ ->
              Expect.equal
                  (parse "{a b c} {d e f}")
                  (Ok(
                      [ Any.Network(
                            Set.ofList
                                [ ElementPattern.Element(Element "a"),
                                  ElementPattern.Element(Element "b"),
                                  ElementPattern.Element(Element "c") ]
                        )


                        Any.Network(
                            Set.ofList
                                [ ElementPattern.Element(Element "d"),
                                  ElementPattern.Element(Element "e"),
                                  ElementPattern.Element(Element "f") ]
                        ) ]
                  ))
                  "" ]
