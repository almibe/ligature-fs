// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Interpreter.Test

open Expecto
open Wander.Tokenizer
open Ligature.Model
open Wander.Model
open Wander.Parser
open Wander.Library

let runScript input =
    match tokenize input with
    | Ok res ->
        match parse res with
        | Ok script -> evalScript stdFns Map.empty script
        | _ -> failwith "TODO"
    | _ -> failwith "Error tokenizing."

[<Tests>]
let tests =
    testList
        "Interpreter Test"
        [ testCase "Run empty script"
          <| fun _ -> Expect.equal (runScript "") (Ok(Map.empty, Any.Network Set.empty)) ""
          testCase "read call with empty network passed"
          <| fun _ -> Expect.equal (runScript "{}") (Ok(Map.empty, Any.Network Set.empty)) ""
          testCase "read call with single count network passed"
          <| fun _ -> Expect.equal (runScript "count {a b c}") (Ok(Map.empty, Any.Term(Term "1"))) ""
          testCase "read call with single count network expression passed"
          <| fun _ -> Expect.equal (runScript "count ({a b c})") (Ok(Map.empty, Any.Term(Term "1"))) "" ]
//   testCase "read network with attribute"
//   <| fun _ ->
//       Expect.equal
//           (parse "{a b \"c\"}")
//           (Ok
//               [ Application
//                     [ Any.Network(
//                           Set.ofList
//                               [ TermPattern.Term(Term "a"),
//                                 TermPattern.Term(Term "b"),
//                                 TermPattern.Term(Term "c") ]
//                       ) ] ])
//           ""
//   testCase "read empty quote"
//   <| fun _ -> Expect.equal (parse "[]") (Ok [ Application [ Any.Quote [] ] ]) ""
//   testCase "read call with pattern passed"
//   <| fun _ ->
//       Expect.equal
//           (parse "{?a b c}")
//           (Ok
//               [ Application
//                     [ Any.Network(
//                           Set.ofList
//                               [ TermPattern.Slot(Slot "?a"),
//                                 TermPattern.Term(Term "b"),
//                                 TermPattern.Term(Term "c") ]
//                       ) ] ])
//           ""
//   testCase "read multiple network script"
//   <| fun _ ->
//       Expect.equal
//           (parse "{a b c}, {d e f}")
//           (Ok
//               [ Application
//                     [ Any.Network(
//                           Set.ofList
//                               [ TermPattern.Term(Term "a"),
//                                 TermPattern.Term(Term "b"),
//                                 TermPattern.Term(Term "c") ]
//                       ) ]
//                 Application
//                     [ Any.Network(
//                           Set.ofList
//                               [ TermPattern.Term(Term "d"),
//                                 TermPattern.Term(Term "e"),
//                                 TermPattern.Term(Term "f") ]
//                       ) ] ])
//           "" ]
