// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.NewMain.Test

open Expecto
open Ligature.Wander.Main
open Ligature.Main
open Ligature.Wander.Lexer
open Ligature.Wander.Parser
open Ligature.LigatureStore.InMemoryStore

[<Tests>]
let tests =
    testList
        "New Main Test"
        [ testCase "Empty script"
          <| fun _ ->
              let script = ""
              let result = run Map.empty emptyInMemoryStore script
              Expect.equal result (Ok None) ""
          testCase "Run Empty Network"
          <| fun _ ->
              let script = "{}"
              let result = run Map.empty emptyInMemoryStore script
              Expect.equal result (Ok None) ""

          testCase "Parse Network"
          <| fun _ ->
              let script = "{a b c}"

              match tokenize script with
              | Ok(res) ->
                  match parse res with
                  | Ok(res) ->
                      Expect.equal
                          res
                          [ LigatureValue.Network(
                                Set.ofList
                                    [ (Pattern.Symbol(Symbol("a")),
                                       Pattern.Symbol(Symbol("b")),
                                       LigatureValue.Symbol(Symbol("c"))) ]
                            ) ]
                          ""
                  | _ -> failwith "Error"
              | _ -> failwith "Error"

          testCase "Parse Named Network"
          <| fun _ ->
              let script = "name {a b c}"

              match tokenize script with
              | Ok(res) ->
                  match parse res with
                  | Ok(res) ->
                      Expect.equal
                          res
                          [ LigatureValue.Symbol(Symbol("name"))
                            LigatureValue.Network(
                                Set.ofList
                                    [ Pattern.Symbol(Symbol("a")),
                                      Pattern.Symbol(Symbol("b")),
                                      LigatureValue.Symbol(Symbol("c")) ]
                            ) ]
                          ""
                  | Error err -> failwith $"Error Parsing {err.UserMessage}"
              | _ -> failwith "Error Tokenizing"

          // testCase "Parse Network With Quote"
          // <| fun _ ->
          //     let script = "{id = [ {} ]}"
          //     failwith "TODO"
          // match tokenize script with
          // | Ok res ->
          //     match parse res with
          //     | Ok res ->
          //         Expect.equal
          //             res
          //             [ LigatureValue.Network
          //                   [ (Pattern.Name(Name("id")),
          //                      ParserElement.Name "=",
          //                      ParserElement.Quote [ ParserElement.Network [] ]) ] ]
          //             ""
          //     | _ -> failwith "Error Parsing"
          // | _ -> failwith "Error Tokenizing"

          //   testCase "Parse Expression"
          //   <| fun _ ->
          //       let script = "(a b @c )"

          //       match tokenize script with
          //       | Ok(res) ->
          //           match parse res with
          //           | Ok(res) ->
          //               Expect.equal
          //                   res
          //                   [ ParserElement.Expression
          //                         [ ParserElement.Name("a"); ParserElement.Name("b"); ParserElement.NetworkName("c") ] ]
          //                   ""

          //               let element = express res []

          //               Expect.equal
          //                   element
          //                   [ Element.Expression
          //                         [ LigatureValue.Name(Name("a"))
          //                           LigatureValue.Name(Name("b"))
          //                           LigatureValue.Name(Name("c")) ] ]
          //                   ""
          //           | _ -> failwith "Error"
          //       | _ -> failwith "Error"

          //   testCase "Parse Nested Expressions"
          //   <| fun _ ->
          //       let script = "(a (b (c) ))"

          //       match tokenize script with
          //       | Ok(res) ->
          //           match parse res with
          //           | Ok(res) ->
          //               Expect.equal
          //                   res
          //                   [ ParserElement.Expression
          //                         [ ParserElement.Name("a")
          //                           ParserElement.Expression
          //                               [ ParserElement.Name("b")
          //                                 ParserElement.Expression [ ParserElement.Name("c") ] ] ] ]
          //                   ""

          //               let element = express res []

          //               Expect.equal
          //                   element
          //                   [ Element.Expression
          //                         [ LigatureValue.Name(Name("a"))
          //                           LigatureValue.Expression
          //                               [ LigatureValue.Name(Name("b"))
          //                                 LigatureValue.Expression [ LigatureValue.Name(Name("c")) ] ] ] ]
          //                   ""
          //           | Error err -> failwith $"Error Parsing - {err.UserMessage}"
          //       | _ -> failwith "Error"

          testCase "Parse Expression Call"
          <| fun _ ->
              let script = "(assert-equal (apply {$a b c} [{$a = b}]) [{b b c }] )"

              match tokenize script with
              | Ok(res) -> Expect.isOk (parse res) ""
              | Error err -> failwith $"Error Parsing - {err.UserMessage}"
              | _ -> failwith "Error"

          //   testCase "Run Network"
          //   <| fun _ ->
          //       let script = "{a b c, e f 89, a b $test, a b @test, a b test.value }"
          //       let result = run Map.empty defaultState script

          //       Expect.equal
          //           result
          //           (Ok(NetworkName(""),
          //               Map.ofList [
          //                 [ defaultNetwork, Set.ofSeq
          //                   [ (Pattern.Name(Name("a")), Pattern.Name(Name("b")), LigatureValue.Name(Name("c")))
          //                     (Pattern.Name(Name("e")), Pattern.Name(Name("f")), LigatureValue.Int(89I))
          //                     (Pattern.Name(Name("a")),
          //                      Pattern.Name(Name("b")),
          //                      LigatureValue.Slot(Slot(Some("test"))))
          //                     (Pattern.Name(Name("a")), Pattern.Name(Name("b")), LigatureValue.NetworkName(NetworkName("test")))
          //                     (Pattern.Name(Name("a")),
          //                      Pattern.Name(Name("b")),
          //                      LigatureValue.Name(Name("test.value"))) ]]
          //               ]

          //           ))
          //           ""

          //   testCase "Run Id Combinator"
          //   <| fun _ ->
          //       let script = "{a b c} id"
          //       let result = run Set.empty script

          //       match result with
          //       | Ok(name, networks) ->
          //           Expect.equal
          //               (currentNetwork (name, networks))
          //               (Set.ofSeq
          //                   [ (Pattern.Name(Name("a")),
          //                      Pattern.Name(Name("b")),
          //                      LigatureValue.Name(Name("c"))) ])
          //               ""
          //       | Error _ -> failwith "Error"
          //   testCase "Run Clear Combinator"
          //   <| fun _ ->
          //       let script = "{a b c} clear"
          //       let result = run stdState script

          //       match result with
          //       | Ok(name, networks) -> Expect.equal (currentNetwork (name, networks)) (Set.ofSeq []) ""
          //       | Error _ -> failwith "Error"

          //   testCase "Run Union Combinator"
          //   <| fun _ ->
          //       let script =
          //           "@l { a b c } @r { d e f } @ { left = @l, right = @r, out = @result } union @result"

          //       let result = run stdState script

          //       match result with
          //       | Ok(name, networks) ->
          //           Expect.equal
          //               (currentNetwork (name, networks))
          //               (Set.ofSeq
          //                   [ (Pattern.Name(Name("a")),
          //                      Pattern.Name(Name("b")),
          //                      LigatureValue.Name(Name("c")))
          //                     (Pattern.Name(Name("d")),
          //                      Pattern.Name(Name("e")),
          //                      LigatureValue.Name(Name("f"))) ])
          //               ""
          //       | Error _ -> failwith "Error"

          //   testCase "Run Minus Combinator"
          //   <| fun _ ->
          //       let script =
          //           "@l { a b c, d e f } @r { d e f, g h i } @ { left = @l, right = @r, out = @result } minus @result"

          //       let result = run Set.empty script //TODO don't use Set.empty, use stdLib

          //       match result with
          //       | Ok(state) ->
          //           Expect.equal
          //               state
          //               (Set.ofSeq
          //                   [ (Pattern.Name(Name("a")),
          //                      Pattern.Name(Name("b")),
          //                      LigatureValue.Name(Name("c"))) ])
          //               ""
          //       | Error err -> failwith $"Error {err}"

          //   testCase "Run Network"
          //   <| fun _ ->
          //       let script = "{a b c, e f 89, a b $test}"
          //       let result = run script emptyState

          //   Expect.equal
          //       result
          //       (Ok(
          //           networkOf (
          //               [ (Pattern.Name(Name("a")), Pattern.Name(Name("b")), Value.Name(Name("c")))
          //                 (Pattern.Name(Name("e")), Pattern.Name(Name("f")), Value.Int(89I))
          //                 (Pattern.Name(Name("a")), Pattern.Name(Name("b")), Value.Slot(Slot(Some("test")))) ]
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
          //                   [ (Pattern.Name(Name("empty")),
          //                      Pattern.Name(Name("=")),
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
          //   testCase "Test running with Names"
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
          //                             [ { Entity = Pattern.Id(ident "a")
          //                                 Attribute = Pattern.Id(ident "b")
          //                                 Value = Value.Name(ident "c") } ]

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
          //                             [ { Entity = Pattern.Id(ident "a")
          //                                 Attribute = Pattern.Id(ident "b")
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
          //                             [ { Entity = Pattern.Id(ident "a")
          //                                 Attribute = Pattern.Id(ident "b")
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
          //                           [ { Entity = Pattern.Id(ident "a")
          //                               Attribute = Pattern.Id(ident "b")
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
          //                           [ { Entity = Pattern.Id(ident "e")
          //                               Attribute = Pattern.Id(ident "a")
          //                               Value = Value.Name(ident "v") } ]
          //                   )
          //               )
          //           ))
          //           ""

          //   testCase "Handle WhiteSpace"
          //   <| fun _ ->
          //     failwith "TODO"
          //   let script = "  \n     "
          //   let result = run Map.empty emptyStore script
          //   Expect.equal result (Ok(emptyStore)) ""
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
          //   testCase "Name concat"
          //   <| fun _ ->
          //       let script = "`a`:`b`"
          //       let result = run script bindings
          //       Expect.equal result (Ok(wident "ab")) ""
          //   testCase "Name concat with String"
          //   <| fun _ ->
          //       let script = "`a`:\"hello\""
          //       let result = run script bindings
          //       Expect.equal result (Ok(wident "ahello")) ""
          //   testCase "Name concat with Int"
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
