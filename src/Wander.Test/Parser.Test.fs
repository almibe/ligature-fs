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
        //   testCase "read empty tuple"
        //   <| fun _ -> Expect.equal (parse "[]") (Ok [ None, Expression.Tuple [] ]) ""
        //   testCase "read tuple"
        //   <| fun _ ->
        //       Expect.equal
        //           (parse "[test \"test2\"]")
        //           (Ok
        //               [ None,
        //                 Expression.Tuple
        //                     [ Expression.Term(Term "test")
        //                       Expression.Element
        //                           { value = "test2"
        //                             space = None
        //                             langTag = None } ] ])
        //           ""
          testCase "read empty node"
          <| fun _ ->
              Expect.equal
                  (parse "p {}")
                  (Ok(
                      [ None,
                        Expression.NodeLiteral
                            { name = Term "p"
                              attributes = Map.empty
                              children = [] } ]
                  ))
                  ""
          testCase "read basic block"
          <| fun _ ->
              Expect.equal
                  (parse "2")
                  (Ok [ None, Expression.Term(Term "2") ])
                  //   ( Expression.NodeExpression
                  //         { name = Term "2"
                  //           attributes = Map.empty
                  //           children = [] } ))
                  "" ]
