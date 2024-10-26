// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Parser.Test

open Expecto
open Wander.Tokenizer
open Ligature.Main
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
                      [ (Symbol "a",
                         [ WanderValue.Network(
                               Set.ofList
                                   [ Entry.Role
                                         { first = Symbol "a"
                                           second = Symbol "c"
                                           role = Symbol "b" } ]
                           ) ]) ]
                  ))
                  ""
          testCase "read call with pattern passed"
          <| fun _ ->
              Expect.equal
                  (parse "a {?a b c}")
                  (Ok(
                      [ (Symbol "a",
                         [ WanderValue.Pattern(
                               Set.ofList
                                   [ QueryTerm.RoleTerm(
                                         Slot.Element(Symbol "a"),
                                         Slot.Element(Symbol "c"),
                                         RoleNameSlot.RoleName(Symbol "b")
                                     ) ]
                           ) ]) ]
                  ))
                  "" ]

//   testCase "read pattern"
//   <| fun _ -> Expect.equal (parse [Token.Variable("?")]) (Ok([ Token.Variable("?") ])) ""
//   testCase "read named variable"
//   <| fun _ -> Expect.equal (tokenize "?test") (Ok([ Token.Variable("?test") ])) ""
//   testCase "return error on invalid input"
//   <| fun _ -> Expect.isError (tokenize "\"") "" ]
