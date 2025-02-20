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
          testCase "read call with empty network passed"
          <| fun _ -> Expect.equal (parse "{}") (Ok [ Application [ Any.Network Set.empty ] ]) ""
          testCase "read call with single count network passed"
          <| fun _ ->
              Expect.equal
                  (parse "{a b c}")
                  (Ok [ Application [ Any.Network(Set.ofList [ (Term "a", Term "b", Term "c") ]) ] ])
                  ""
          testCase "read network with attribute"
          <| fun _ ->
              Expect.equal
                  (parse "{a b \"c\"}")
                  (Ok [ Application [ Any.Network(Set.ofList [ Term "a", Term "b", Term "c" ]) ] ])
                  ""
          testCase "read empty quote"
          <| fun _ -> Expect.equal (parse "[]") (Ok [ Application [ Any.Quote [] ] ]) ""
          testCase "read basic block"
          <| fun _ ->
              Expect.equal (parse "(2)") (Ok [ Application [ Any.Block [ Application [ Any.Term(Term "2") ] ] ] ]) ""
          testCase "read call with pattern passed"
          <| fun _ ->
              Expect.equal
                  (parse "{?a b c}")
                  (Ok
                      [ Application
                            [ Any.Pattern(
                                  Set.ofList
                                      [ TermPattern.Slot(Slot "?a"),
                                        TermPattern.Term(Term "b"),
                                        TermPattern.Term(Term "c") ]
                              ) ] ])
                  ""
          testCase "read multiple network script"
          <| fun _ ->
              Expect.equal
                  (parse "{a b c}, {d e f}")
                  (Ok
                      [ Application [ Any.Network(Set.ofList [ Term "a", Term "b", Term "c" ]) ]
                        Application [ Any.Network(Set.ofList [ Term "d", Term "e", Term "f" ]) ] ])
                  "" ]
