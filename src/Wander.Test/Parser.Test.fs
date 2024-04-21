// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Parser.Test

open Expecto
open Ligature
open Wander.Parser
open Wander.Lexer

let unsafe result =
    match result with
    | Ok(v) -> v
    | Error(_) -> failwith "Error"

let ident id =
    match identifier id with
    | Ok(v) -> v
    | Error(_) -> failwith "todo"

[<Tests>]
let tests =
    testList
        "Parser Suite"
        [ testCase "Parse Integer"
          <| fun _ ->
              let tokens = [ Token.Int(345I) ]
              let elements = parse tokens
              Expect.equal elements (Ok([ Element.Int(345I) ])) ""
          testCase "Parse Bytes"
          <| fun _ ->
              let tokens = tokenize "0xFF"
              let elements = parse (unsafe tokens)
              Expect.equal elements (Ok([ Element.Bytes([| 0xFFuy |]) ])) ""
          testCase "Parse Bool"
          <| fun _ ->
              let tokens = tokenize "true"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Bool(true) ])) ""
          testCase "Parse Identifier"
          <| fun _ ->
              let tokens = tokenize "`hello`"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Identifier(ident "hello") ])) ""
          testCase "Parse String"
          <| fun _ ->
              let tokens = tokenize "\"hello\""
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.String("hello") ])) ""
          testCase "Parse Let Statements"
          <| fun _ ->
              let tokens = tokenize "x = 5"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Let("x", Element.Int(5I)) ])) ""
          testCase "Parse Name"
          <| fun _ ->
              let tokens = tokenize "hello"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.NamePath([ "hello" ]) ])) ""
          testCase "Parse NamePath"
          <| fun _ ->
              let tokens = tokenize "hello.world"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.NamePath([ "hello"; "world" ]) ])) ""
          testCase "Parse Empty Array"
          <| fun _ ->
              let tokens = tokenize "[]"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Array([]) ])) ""
          testCase "Parse Array with 1 element"
          <| fun _ ->
              let tokens = tokenize "[1]"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Array([ Element.Int(1I) ]) ])) ""
          testCase "Parse Array with 2 elements"
          <| fun _ ->
              let tokens = tokenize "[1, 2]"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Array([ Element.Int(1I); Element.Int(2I) ]) ])) ""

          testCase "Parse Dataset Literal"
          <| fun _ ->
              let tokens = tokenize "{ `a` `b` `c` }"
              let ast = parse (unsafe tokens)

              Expect.equal
                  ast
                  (Ok(
                      [ Element.Dataset(
                            [ (Element.Identifier(ident "a"),
                               Element.Identifier(ident "b"),
                               [ Element.Identifier(ident "c") ]) ]
                        ) ]
                  ))
                  ""

          testCase "Parse Dataset Literal with two Statements"
          <| fun _ ->
              let tokens = tokenize "{ `a` `b` `c`, `d` `e` `f` }"
              let ast = parse (unsafe tokens)

              Expect.equal
                  ast
                  (Ok(
                      [ Element.Dataset(
                            [ (Element.Identifier(ident "a"),
                               Element.Identifier(ident "b"),
                               [ Element.Identifier(ident "c") ])
                              (Element.Identifier(ident "d"),
                               Element.Identifier(ident "e"),
                               [ Element.Identifier(ident "f") ]) ]
                        ) ]
                  ))
                  ""

          testCase "Parse Dataset Literal with Value list"
          <| fun _ ->
              let tokens = tokenize "{ `a` `b` [`c`, `d`] }"
              let ast = parse (unsafe tokens)

              Expect.equal
                  ast
                  (Ok(
                      [ Element.Dataset(
                            [ (Element.Identifier(ident "a"),
                               Element.Identifier(ident "b"),
                               [ Element.Identifier(ident "c"); Element.Identifier(ident "d") ])
                              ]
                        ) ]
                  ))
                  ""

          testCase "Parse Dataset Literal with Entity expansion"
          <| fun _ ->
              let tokens = tokenize "{ `a` `b` `c` { `d` `e` } }"
              let ast = parse (unsafe tokens)

              Expect.equal
                  ast
                  (Ok(
                      [ Element.Dataset(
                            [ (Element.Identifier(ident "a"),
                               Element.Identifier(ident "b"),
                               [ Element.Identifier(ident "c") ])
                              (Element.Identifier(ident "c"),
                               Element.Identifier(ident "d"),
                               [ Element.Identifier(ident "e") ]) ]
                        ) ]
                  ))
                  ""

          testCase "Parse Empty Dataset"
          <| fun _ ->
              let tokens = tokenize "{ }"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Dataset([]) ])) ""

          testCase "Parse Record with 1 value"
          <| fun _ ->
              let tokens = tokenize "{ x = 5 }"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Record([ ("x", Element.Int(5I)) ]) ])) ""
          testCase "Parse Record with 2 values"
          <| fun _ ->
              let tokens = tokenize "{ x = 5, y = false }"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Record([ ("x", Element.Int(5I)); ("y", Element.Bool(false)) ]) ])) ""
          testCase "Parse Lambda"
          <| fun _ ->
              let tokens = tokenize "\\x -> x"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Lambda([ "x" ], Element.NamePath([ "x" ])) ])) ""
          testCase "Parse Application"
          <| fun _ ->
              let tokens = tokenize "Bool.and false true"
              let ast = parse (unsafe tokens)

              Expect.equal
                  ast
                  (Ok(
                      [ Element.Application(
                            [ Element.NamePath([ "Bool"; "and" ]); Element.Bool(false); Element.Bool(true) ]
                        ) ]
                  ))
                  ""
          testCase "Parse Let Statement with Name"
          <| fun _ ->
              let tokens = tokenize "x = 5, x"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Let("x", Element.Int(5I)); Element.NamePath([ "x" ]) ])) ""
          testCase "Parse Grouping"
          <| fun _ ->
              let tokens = tokenize "(x = 5, x)"
              let ast = parse (unsafe tokens)

              Expect.equal
                  ast
                  (Ok([ Element.Grouping([ Element.Let("x", Element.Int(5I)); Element.NamePath([ "x" ]) ]) ]))
                  ""
          testCase "Parse Match Expression"
          <| fun _ ->
              let tokens = tokenize "match x * x -> x * y -> y"
              let ast = parse (unsafe tokens)

              Expect.equal
                  ast
                  (Ok(
                      [ Element.Match(
                            Element.NamePath([ "x" ]),
                            [ (Element.NamePath([ "x" ]), Element.NamePath([ "x" ]))
                              (Element.NamePath([ "y" ]), Element.NamePath([ "y" ])) ]
                        ) ]
                  ))
                  ""

          testCase "Parse Match Expression With Additional Types"
          <| fun _ ->
              let tokens = tokenize "match x * true -> 5 * false -> 6"
              let ast = parse (unsafe tokens)

              Expect.equal
                  ast
                  (Ok(
                      [ Element.Match(
                            Element.NamePath([ "x" ]),
                            [ (Element.Bool(true), Element.Int(5I))
                              (Element.Bool(false), Element.Int(6I)) ]
                        ) ]
                  ))
                  ""

          //   testCase "Parse Match Expression 3"
          //   <| fun _ ->
          //       let tokens = tokenize "match {`a` `b` `c`} * false -> 6"
          //       let ast = parse (unsafe tokens)

          //       Expect.equal
          //           ast
          //           (Ok([ Element.Match(Element.NamePath(["x"]), [ (Element.Bool(true), Element.Int(5)); (Element.Bool(false), Element.Int(6)) ]) ]))
          //           ""


          testCase "Parse pipes"
          <| fun _ ->
              let tokens = tokenize "|||"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Application [ Element.Pipe; Element.Pipe; Element.Pipe ] ])) ""
          testCase "Parse question marks"
          <| fun _ ->
              let tokens = tokenize "? ?"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Application [ Element.NamePath [ "?" ]; Element.NamePath [ "?" ] ] ])) ""
          testCase "Parse identifier concat"
          <| fun _ ->
              let tokens = tokenize "`a`:`b`"
              let ast = parse (unsafe tokens)

              Expect.equal
                  ast
                  (Ok(
                      [ Element.Application
                            [ Element.Identifier(ident "a"); Element.Colon; Element.Identifier(ident "b") ] ]
                  ))
                  ""

          //   testCase "Parse Integers"
          //   <| fun _ ->
          //       Expect.equal (parseString "123") (Ok([ Expression.Value(WanderValue.Integer(123)) ])) ""
          //       Expect.equal (parseString "0") (Ok([ Expression.Value(WanderValue.Integer(0)) ])) ""
          //       Expect.equal (parseString "-4123") (Ok([ Expression.Value(WanderValue.Integer(-4123)) ])) ""
          //   testCase "Read Names"
          //   <| fun _ -> Expect.equal (parseString "hello") (Ok([ Expression.Name("hello") ])) ""
          //   testCase "parse booleans"
          //   <| fun _ ->
          //       Expect.equal (parseString "true") (Ok([ Expression.Value(WanderValue.Boolean(true)) ])) ""
          //       Expect.equal (parseString "false") (Ok([ Expression.Value(WanderValue.Boolean(false)) ])) ""

          //       Expect.equal
          //           (parseString "true false    true \n false false")
          //           (Ok(
          //               [ Expression.Value(WanderValue.Boolean(true))
          //                 Expression.Value(WanderValue.Boolean(false))
          //                 Expression.Value(WanderValue.Boolean(true))
          //                 Expression.Value(WanderValue.Boolean(false))
          //                 Expression.Value(WanderValue.Boolean(false)) ]
          //           ))
          //           ""
          //   testCase "parse whitespace"
          //   <| fun _ ->
          //       Expect.equal (parseString " ") (Ok([])) ""
          //       Expect.equal (parseString "   ") (Ok([])) ""
          //   //Expect.equal (parse "\t") (Ok([WhiteSpace("\t")])) ""
          //   //Expect.equal (parse "\t  ") (Ok([WhiteSpace("\t  ")])) ""
          //   testCase "parse new lines"
          //   <| fun _ ->
          //       Expect.equal (parseString "\n") (Ok([])) ""
          //       Expect.equal (parseString "\r\n") (Ok([])) ""
          //       Expect.equal (parseString "\r\n\r\n\r\n\n") (Ok([])) ""
          //   testCase "Read Identifiers"
          //   <| fun _ ->
          //       Expect.equal (parseString "<a>") (Ok([ Expression.Value(ident "a") ])) ""
          //       Expect.equal (parseString "<https://ligature.dev/#>") (Ok([ Expression.Value(ident "https://ligature.dev/#") ])) ""
          //   testCase "Read comments"
          //   <| fun _ ->
          //       Expect.equal (parseString "--") (Ok([])) ""
          //       Expect.equal (parseString "--hello") (Ok([])) ""
          //       Expect.equal (parseString "-- this is a@#$@%$#@$%@ comment;;;;  ") (Ok([])) ""
          //       Expect.equal (parseString "-- this is \n--  a@#$@%$#@$%@ comment;;;;  ") (Ok([])) ""
          //   testCase "read String Literal"
          //   <| fun _ -> Expect.equal (parseString @"""hello""") (Ok([ Expression.Value(WanderValue.String("hello")) ])) ""
          //   // // testCase "read Bytes Literal" <| fun _ ->
          //   // //     Expect.equal (parse "0x55") (Ok([Bytes("0x55")])) ""
          //   testCase "read let statement"
          //   <| fun _ ->
          //       Expect.equal (parseString "let x = 6") (Ok([ Expression.LetStatement("x", Expression.Value(WanderValue.Integer(6))) ])) ""
          //       Expect.equal (parseString "let x=8") (Ok([ Expression.LetStatement("x", Expression.Value(WanderValue.Integer(8))) ])) ""
          //       Expect.equal (parseString "let x = \n true") (Ok([ Expression.LetStatement("x", Expression.Value(WanderValue.Boolean(true))) ])) ""
          //       Expect.equal (parseString @"let x = ""true""") (Ok([ Expression.LetStatement("x", Expression.Value(WanderValue.String("true"))) ])) ""
          //       Expect.equal (parseString "let x = <a>") (Ok([ Expression.LetStatement("x", Expression.Value(ident "a")) ])) ""
          //   testCase "read Scopes"
          //   <| fun _ ->
          //       Expect.equal (parseString "{ true }") (Ok [ Expression.Scope [ Expression.Value(WanderValue.Boolean(true)) ] ]) ""
          //       Expect.equal (parseString "{ 55 }") (Ok [ Expression.Scope [ Expression.Value(WanderValue.Integer(55)) ] ]) ""

          //       Expect.equal
          //           (parseString @"{ 1 true 3 ""Hello""}")
          //           (Ok
          //               [ Expression.Scope
          //                     [ Expression.Value(WanderValue.Integer(1))
          //                       Expression.Value(WanderValue.Boolean(true))
          //                       Expression.Value(WanderValue.Integer(3))
          //                       Expression.Value(WanderValue.String("Hello")) ] ])
          //           ""
          //   testCase "read let with scope"
          //   <| fun _ ->
          //       Expect.equal
          //           (parseString "let x = { true }")
          //           (Ok([ Expression.LetStatement("x", Expression.Scope([ Expression.Value(WanderValue.Boolean(true)) ])) ]))
          //           ""

          //       Expect.equal (parseString "{ let x = 6 }") (Ok([ Expression.Scope([ Expression.LetStatement("x", Expression.Value(WanderValue.Integer(6))) ]) ])) ""

          //       Expect.equal
          //           (parseString "{ let x = { false } x }")
          //           (Ok([ Expression.Scope([ Expression.LetStatement("x", Expression.Scope([ Expression.Value(WanderValue.Boolean(false)) ])); Expression.Name("x") ]) ]))
          //           ""
          //   testCase "parse conditionals"
          //   <| fun _ ->
          //       Expect.equal
          //           (parseString "if true false else true")
          //           (Ok(
          //               [ Expression.Conditional
          //                     { ifCase =
          //                         { condition = Expression.Value(WanderValue.Boolean(true))
          //                           body = Expression.Value(WanderValue.Boolean(false)) }
          //                       elsifCases = []
          //                       elseBody = Expression.Value(WanderValue.Boolean(true)) } ]
          //           ))
          //           ""
          //   testCase "parsing function calls"
          //   <| fun _ ->
          //       Expect.equal (parseString "hello()") (Ok([Expression.FunctionCall("hello", [])])) ""
          //       Expect.equal (parseString @"hello(""world"")") (Ok([Expression.FunctionCall("hello", [Expression.Value(WanderValue.String"world")])])) ""
          //       Expect.equal (parseString "let four = add(1 3)") (Ok([
          //         Expression.LetStatement("four", Expression.FunctionCall("add", [Expression.Value(WanderValue.Integer(1)); Expression.Value(WanderValue.Integer(3))]))
          //       ])) ""
          //   testCase "parsing lambdas"
          //   <| fun _ ->
          //       Expect.equal (parseString "{ -> }") (Ok([Expression.Value(WanderValue.Lambda([], []))])) ""
          //       Expect.equal (parseString "{ x -> x }") (Ok([Expression.Value(WanderValue.Lambda(["x"], [Expression.Name("x")]))])) ""
          //       Expect.equal (parseString "{ x -> func(x 3) }") (Ok([Expression.Value(WanderValue.Lambda(["x"], [Expression.FunctionCall("func", [Expression.Name("x"); Expression.Value(WanderValue.Integer(3L))])]))])) ""
          //       Expect.equal (parseString "let addThree = { x -> add(3 x) }") (Ok([
          //         Expression.LetStatement("addThree", Expression.Value(WanderValue.Lambda(["x"], [Expression.FunctionCall("add", [Expression.Value(WanderValue.Integer(3L)); Expression.Name("x")])])))
          //       ])) ""
          //   testCase "parsing tuples"
          //   <| fun _ ->
          //       Expect.equal (parseString "()") (Ok([Expression.TupleExpression([])])) "Parse Empty Tuple"
          //       Expect.equal (parseString "(x)") (Ok([Expression.TupleExpression([Expression.Name("x")])])) ""
          //       Expect.equal (parseString "(x 3)") (Ok([Expression.TupleExpression([Expression.Name("x"); Expression.Value(WanderValue.Integer(3))])])) ""
          //       Expect.equal (parseString "let x3 = (x 3)") (Ok([
          //         Expression.LetStatement("x3", Expression.TupleExpression([Expression.Name("x"); Expression.Value(WanderValue.Integer(3L))]))
          //       ])) ""
          ]
