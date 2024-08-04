// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.NewMain.Test

open Expecto
open Ligature.Wander.Model
open Ligature.Wander.Main
open Ligature.Main
open Ligature.InMemoryNetwork
open Ligature.Wander.Interpreter
open Ligature.Wander.Lexer
open Ligature.Wander.Parser
//open Ligature.Wander.Lib.Lib

[<Tests>]
let tests =
    testList
        "New Main Test"
        [ testCase "Empty script"
          <| fun _ ->
              let script = ""
              let result = run Map.empty defaultState script
              Expect.equal result (Ok(defaultState)) ""
          testCase "Run Empty Network"
          <| fun _ ->
              let script = "{}"
              let result = run Map.empty defaultState script
              Expect.equal result (Ok(defaultState)) ""

          testCase "Parse Network"
          <| fun _ ->
              let script = "{a b c}"

              match tokenize script with
              | Ok(res) ->
                  match parse res with
                  | Ok(res) ->
                      Expect.equal
                          res
                          [ Element.Network [ (Element.Word("a"), Element.Word("b"), Element.Word("c")) ] ]
                          ""
                  | _ -> failwith "Error"
              | _ -> failwith "Error"

          testCase "Parse Network With Quote"
          <| fun _ ->
              let script = "{id = [ x ]}"

              match tokenize script with
              | Ok(res) ->
                  match parse res with
                  | Ok(res) ->
                      Expect.equal
                          res
                          [ Element.Network
                                [ (Element.Word("id"), Element.Word("="), Element.Quote([], [ Element.Call("x", []) ])) ] ]
                          ""
                  | _ -> failwith "Error"
              | _ -> failwith "Error"

          testCase "Parse Named Network"
          <| fun _ ->
              let script = "@name {a b c}"

              match tokenize script with
              | Ok(res) ->
                  match parse res with
                  | Ok(res) ->
                      Expect.equal
                          res
                          [ Element.NetworkName("name")
                            Element.Network [ Element.Word("a"), Element.Word("b"), Element.Word("c") ] ]
                          ""
                  | _ -> failwith "Error Parsing"
              | _ -> failwith "Error Tokenizing"

          testCase "Run Network"
          <| fun _ ->
              let script = "{a b c, e f 89, a b $test}"
              let result = run Map.empty defaultState script
              Expect.equal
                  result
                  (Ok((NetworkName(""),
                      Map.ofSeq [(NetworkName(""),
                      
                      Set.ofSeq [ 
                          (PatternWord.Word(Word("a")), PatternWord.Word(Word("b")), LigatureValue.Word(Word("c")))
                          (PatternWord.Word(Word("e")), PatternWord.Word(Word("f")), LigatureValue.Int(89I))
                          (PatternWord.Word(Word("a")),
                          PatternWord.Word(Word("b")),
                          LigatureValue.Slot(Slot(Some("test")))) ]
                      )]
                  )))
                  ""

          testCase "Run Named Network"
          <| fun _ ->
              let script = "@test {a b c}"
              let result = run Map.empty defaultState script
              Expect.equal
                  result
                  (Ok((NetworkName("test"),
                      Map.ofSeq [ 
                        (NetworkName "", Set.empty)
                        (NetworkName "test",
                        Set.ofSeq
                            [ (PatternWord.Word(Word("a")), PatternWord.Word(Word("b")), LigatureValue.Word(Word("c"))) ]                      
                      ) ]
                  )))
                  ""

          //   testCase "Define 'call' Word with Parameters"
          //   <| fun _ ->
          //       let script = "{ call = [ x ] } call [ {a b c} ]"
          //       let result = run Map.empty emptyState script

          //       Expect.equal
          //           result
          //           (Ok(
          //               Set.ofSeq [
          //                 (PatternWord.Word(Word("call")), PatternWord.Word(Word("=")), LigatureValue.Pipeline([LigatureValue.Word(Word("x"))]));
          //                 (PatternWord.Word(Word("a")), PatternWord.Word(Word("b")), LigatureValue.Word(Word("c"))) ]
          //           ))
          //           ""

          //   testCase "Run Network"
          //   <| fun _ ->
          //       let script = "{a b c, e f 89, a b $test}"
          //       let result = run script emptyState

          //   Expect.equal
          //       result
          //       (Ok(
          //           networkOf (
          //               [ (PatternWord.Word(Word("a")), PatternWord.Word(Word("b")), Value.Word(Word("c")))
          //                 (PatternWord.Word(Word("e")), PatternWord.Word(Word("f")), Value.Int(89I))
          //                 (PatternWord.Word(Word("a")), PatternWord.Word(Word("b")), Value.Slot(Slot(Some("test")))) ]
          //           )
          //   ))
          //   ""
          //   testCase "Run Network with Quote"
          //   <| fun _ ->
          //       let script = "{empty = []}"
          //       let result = run script emptyState

          //       Expect.equal
          //           result
          //           (Ok(
          //               networkOf (
          //                   [ (PatternWord.Word(Word("empty")),
          //                      PatternWord.Word(Word("=")),
          //                      Value.Quote({ parameters = []; value = [] })) ]
          //               )
          //           ))
          //           ""
          //   testCase "Run Slot"
          //   <| fun _ ->
          //       let script = "$hello"
          //       let result = run script emptyState
          //       Expect.equal result (Ok([ WanderValue.Slot(Slot(Some("hello"))) ])) ""
          //   testCase "Run Empty Network literal"
          //   <| fun _ ->
          //       let script = "{}"
          //       let result = run script Map.empty List.empty
          //       Expect.equal result (Ok([ WanderValue.Network(InMemoryNetwork(Set.empty)) ])) ""
          //   testCase "Run Empty Quote Literal"
          //   <| fun _ ->
          //       let script = "[]"
          //       let result = run script emptyState
          //       Expect.equal result (Ok([ WanderValue.Quote([]) ])) ""
          //   testCase "Run Quote Literal"
          //   <| fun _ ->
          //       let script = "[1 `test`]"
          //       let result = run script emptyState
          //       Expect.equal result (Ok([ WanderValue.Quote([ WanderValue.Int(1I); wident "test" ]) ])) ""
          //   testCase "Test running with Words"
          //   <| fun _ ->
          //       let script = "1 2 pop"
          //       let result = run script stdLib List.empty
          //       Expect.equal result (Ok([ WanderValue.Int(1I) ])) ""
          //   testCase "Test apply combinator"
          //   <| fun _ ->
          //       let script = "[1 2] apply"
          //       let result = run script stdLib List.empty
          //       Expect.equal result (Ok([ WanderValue.Int(2I); WanderValue.Int(1I) ])) ""
          //   testCase "Test apply combinator with another combinator"
          //   <| fun _ ->
          //       let script = "[1 2 pop] apply"
          //       let result = run script stdLib List.empty
          //       Expect.equal result (Ok([ WanderValue.Int(1I) ])) ""
          //   testCase "Run Dataset literal"
          //   <| fun _ ->
          //       let script = "{`a` `b` `c`}"
          //       let result = run script Map.empty List.empty

          //       Expect.equal
          //           result
          //           (Ok(
          //               [ WanderValue.Network(
          //                     InMemoryNetwork(
          //                         Set.ofSeq
          //                             [ { Entity = PatternWord.Id(ident "a")
          //                                 Attribute = PatternWord.Id(ident "b")
          //                                 Value = Value.Identifier(ident "c") } ]

          //                     )
          //                 ) ]
          //           ))
          //           ""
          //   testCase "Run Dataset literal with Int"
          //   <| fun _ ->
          //       let script = "{`a` `b` 5}"
          //       let result = run script Map.empty List.empty

          //       Expect.equal
          //           result
          //           (Ok(
          //               [ WanderValue.Network(
          //                     InMemoryNetwork(
          //                         Set.ofSeq
          //                             [ { Entity = PatternWord.Id(ident "a")
          //                                 Attribute = PatternWord.Id(ident "b")
          //                                 Value = Value.Int(5I) } ]

          //                     )
          //                 ) ]
          //           ))
          //           ""
          //   testCase "Run Dataset literal with String"
          //   <| fun _ ->
          //       let script = "{`a` `b` \"Hi\"}"
          //       let result = run script Map.empty List.empty

          //       Expect.equal
          //           result
          //           (Ok(
          //               [ WanderValue.Network(
          //                     InMemoryNetwork(
          //                         Set.ofSeq
          //                             [ { Entity = PatternWord.Id(ident "a")
          //                                 Attribute = PatternWord.Id(ident "b")
          //                                 Value = Value.String("Hi") } ]
          //                     )
          //                 )

          //                 ]
          //           ))
          //           ""
          //   testCase "Run Dataset literal with Bytes"
          //   <| fun _ ->
          //       let script = "{`a` `b` 0x00}"
          //       let result = run script bindings

          //       Expect.equal
          //           result
          //           (Ok(
          //               WanderValue.Network(
          //                   InMemoryNetwork(
          //                       Set.ofSeq
          //                           [ { Entity = PatternWord.Id(ident "a")
          //                               Attribute = PatternWord.Id(ident "b")
          //                               Value = Value.Bytes([| 0uy |]) } ]
          //                   )
          //               )
          //           ))
          //           ""
          //   testCase "Run Dataset literal with Names"
          //   <| fun _ ->
          //       let script = "x = `e`, y = `a`, z= `v`, {x y `v`}"
          //       let result = run script bindings

          //       Expect.equal
          //           result
          //           (Ok(
          //               WanderValue.Network(
          //                   InMemoryNetwork(
          //                       Set.ofSeq
          //                           [ { Entity = PatternWord.Id(ident "e")
          //                               Attribute = PatternWord.Id(ident "a")
          //                               Value = Value.Identifier(ident "v") } ]
          //                   )
          //               )
          //           ))
          //           ""

          testCase "Handle WhiteSpace"
          <| fun _ ->
              let script = "  \n     "
              let result = run Map.empty defaultState script
              Expect.equal result (Ok(defaultState)) ""
          //   testCase "Handle Multiple Values and White Space"
          //   <| fun _ ->
          //       let script = " 1  \n `a` \"hello\" \r\n  321 \n"
          //       let result = run script Map.empty List.empty

          //       Expect.equal
          //           result
          //           (Ok(
          //               [ WanderValue.Int(321I)
          //                 WanderValue.String("hello")
          //                 wident "a"
          //                 WanderValue.Int(1I) ]
          //           ))
          //           ""
          //   testCase "Let Triple"
          //   <| fun _ ->
          //       let script = "x = 5"
          //       let result = run script bindings
          //       Expect.equal result (Ok(WanderValue.Int(5I))) ""
          //   testCase "Let Triple with Value Reference"
          //   <| fun _ ->
          //       let script = "x = 5,\nx"
          //       let result = run script bindings
          //       Expect.equal result (Ok(WanderValue.Int(5I))) ""
          //   testCase "Identifier concat"
          //   <| fun _ ->
          //       let script = "`a`:`b`"
          //       let result = run script bindings
          //       Expect.equal result (Ok(wident "ab")) ""
          //   testCase "Identifier concat with String"
          //   <| fun _ ->
          //       let script = "`a`:\"hello\""
          //       let result = run script bindings
          //       Expect.equal result (Ok(wident "ahello")) ""
          //   testCase "Identifier concat with Int"
          //   <| fun _ ->
          //       let script = "`a`:123"
          //       let result = run script bindings
          //       Expect.equal result (Ok(wident "a123")) ""

          //   testCase "Let Triple with Value Reference In Scope"
          //   <| fun _ ->
          //       let script = "(let x 5, x)"
          //       let result = run script bindings
          //       Expect.equal result (Ok(WanderValue.Int(5))) ""
          //   testCase "Let Triple with Value Reference Outside Scope"
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
