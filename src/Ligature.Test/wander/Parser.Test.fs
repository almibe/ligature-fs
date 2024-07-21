// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Parser.Test

open Expecto
open Ligature.Main
open Ligature.Wander.Parser
open Ligature.Wander.Lexer

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
          testCase "Parse Identifier"
          <| fun _ ->
              let tokens = tokenize "`hello`"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Identifier(ident "hello") ])) ""
          testCase "Parse Slot"
          <| fun _ ->
              let tokens = tokenize "$hello"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Slot(Slot(Some "hello")) ])) ""

          testCase "Parse String"
          <| fun _ ->
              let tokens = tokenize "\"hello\""
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.String("hello") ])) ""
          testCase "Parse Let Triples"
          <| fun _ ->
              let tokens = tokenize "x = 5"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Definition("x", Element.Int(5I)) ])) ""
          testCase "Parse Word"
          <| fun _ ->
              let tokens = tokenize "hello"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Word("hello") ])) ""
          testCase "Parse WordPath"
          <| fun _ ->
              let tokens = tokenize "hello.world"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Word("hello.world") ])) ""
          testCase "Parse Empty Quote"
          <| fun _ ->
              let tokens = tokenize "[]"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Quote([]) ])) ""
          testCase "Parse Quote with 1 element"
          <| fun _ ->
              let tokens = tokenize "[1]"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Quote([ Element.Int(1I) ]) ])) ""
          testCase "Parse Quote with 2 elements"
          <| fun _ ->
              let tokens = tokenize "[1 2]"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Quote([ Element.Int(1I); Element.Int(2I) ]) ])) ""

          //   testCase "Parse Dataset Literal"
          //   <| fun _ ->
          //       let tokens = tokenize "{ `a` `b` `c` }"
          //       let ast = parse (unsafe tokens)

          //       Expect.equal
          //           ast
          //           (Ok(
          //               [ Element.Network(
          //                     [ (Element.Identifier(ident "a"),
          //                        [ (Element.Identifier(ident "b"), [ Element.Identifier(ident "c") ]) ]) ]
          //                 ) ]
          //           ))
          //           ""

          //   testCase "Parse Dataset Literal with two Triples"
          //   <| fun _ ->
          //       let tokens = tokenize "{ `a` `b` `c`, `d` `e` `f` }"
          //       let ast = parse (unsafe tokens)

          //       Expect.equal
          //           ast
          //           (Ok(
          //               [ Element.Network(
          //                     [ (Element.Identifier(ident "a"),
          //                        [ (Element.Identifier(ident "b"), [ Element.Identifier(ident "c") ]) ])
          //                       (Element.Identifier(ident "d"),
          //                        [ (Element.Identifier(ident "e"), [ Element.Identifier(ident "f") ]) ]) ]
          //                 ) ]
          //           ))
          //           ""

          //   testCase "Parse Dataset Literal with Value list"
          //   <| fun _ ->
          //       let tokens = tokenize "{ `a` `b` [`c`, `d`] }"
          //       let ast = parse (unsafe tokens)

          //       Expect.equal
          //           ast
          //           (Ok(
          //               [ Element.Network(
          //                     [ (Element.Identifier(ident "a"),
          //                        [ (Element.Identifier(ident "b"),
          //                           [ Element.Identifier(ident "c"); Element.Identifier(ident "d") ]) ]) ]
          //                 ) ]
          //           ))
          //           ""

          //   testCase "Parse Network with all wildcards"
          //   <| fun _ ->
          //       let tokens = tokenize "{ $ $ $ }"
          //       let ast = parse (unsafe tokens)

          //       Expect.equal
          //           ast
          //           (Ok(
          //               [ Element.Network(
          //                     [ (Element.Slot(Slot(None)), [ (Element.Slot(Slot(None)), [ Element.Slot(Slot(None)) ]) ]) ]
          //                 ) ]
          //           ))
          //           ""

          //   testCase "Parse Network with all Wordd wildcards"
          //   <| fun _ ->
          //       let tokens = tokenize "{ $entity $attribute $value }"
          //       let ast = parse (unsafe tokens)

          //       Expect.equal
          //           ast
          //           (Ok(
          //               [ Element.Network(
          //                     [ (Element.Slot(Slot(Some("entity")))),
          //                       [ (Element.Slot(Slot(Some("attribute")))), [ Element.Slot(Slot(Some("value"))) ] ] ]
          //                 ) ]
          //           ))
          //           ""


          //   testCase "Parse Empty Network"
          //   <| fun _ ->
          //       let tokens = tokenize "{ }"
          //       let ast = parse (unsafe tokens)
          //       Expect.equal ast (Ok([ Element.Network([]) ])) ""

          testCase "Parse Assoc Array with 1 value"
          <| fun _ ->
              let tokens = tokenize "[ x = 5 ]"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.AssocArray([ ("x", Element.Int(5I)) ]) ])) ""
          testCase "Parse Assoc Array with 2 values"
          <| fun _ ->
              let tokens = tokenize "[ x = 5, y = \"false\" ]"
              let ast = parse (unsafe tokens)

              Expect.equal
                  ast
                  (Ok([ Element.AssocArray([ ("x", Element.Int(5I)); ("y", Element.String("false")) ]) ]))
                  ""

          //   testCase "Parse Application"
          //   <| fun _ ->
          //       let tokens = tokenize "Bool.and false true"
          //       let ast = parse (unsafe tokens)

          //       Expect.equal
          //           ast
          //           (Ok([ Element.Application([ Element.Word("Bool.and"); Element.Bool(false); Element.Bool(true) ]) ]))
          //           ""
          //   testCase "Parse Let Triple with Word"
          //   <| fun _ ->
          //       let tokens = tokenize "x = 5, x"
          //       let ast = parse (unsafe tokens)
          //       Expect.equal ast (Ok([ Element.Definition("x", Element.Int(5I)); Element.Word("x") ])) ""
          testCase "Parse Two Literals in a Row"
          <| fun _ ->
              let tokens = tokenize "1 2"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Int(1I); Element.Int(2I) ])) ""

          //   testCase "Parse Quote"
          //   <| fun _ ->
          //       let tokens = tokenize "(x = 5, x)"
          //       let ast = parse (unsafe tokens)

          //       Expect.equal ast (Ok([ Element.Quote([ Element.Definition("x", Element.Int(5I)); Element.Word("x") ]) ])) ""

          //   testCase "Parse pipes"
          //   <| fun _ ->
          //       let tokens = tokenize "|||"
          //       let ast = parse (unsafe tokens)
          //       Expect.equal ast (Ok([ Element.Application [ Element.Pipe; Element.Pipe; Element.Pipe ] ])) ""
          //   testCase "Parse question marks"
          //   <| fun _ ->
          //       let tokens = tokenize "? ?"
          //       let ast = parse (unsafe tokens)
          //       Expect.equal ast (Ok([ Element.Application [ Element.Word("?"); Element.Word("?") ] ])) ""
          //   testCase "Parse identifier concat"
          //   <| fun _ ->
          //       let tokens = tokenize "`a`:`b`"
          //       let ast = parse (unsafe tokens)

          //       Expect.equal
          //           ast
          //           (Ok(
          //               [ Element.Application
          //                     [ Element.Identifier(ident "a"); Element.Colon; Element.Identifier(ident "b") ] ]
          //           ))
          //           ""
          ]
