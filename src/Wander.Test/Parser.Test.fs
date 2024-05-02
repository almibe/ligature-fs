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

let slot id =
    match slot (Some id) with
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
          testCase "Parse Slot"
          <| fun _ ->
              let tokens = tokenize "$hello"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Slot(slot "hello") ])) ""

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
              Expect.equal ast (Ok([ Element.Name("hello") ])) ""
          testCase "Parse NamePath"
          <| fun _ ->
              let tokens = tokenize "hello.world"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Name("hello.world") ])) ""
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
                      [ Element.Pattern(
                            [ (Element.Identifier(ident "a"),
                               [ (Element.Identifier(ident "b"), [ Element.Identifier(ident "c") ]) ]) ]
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
                      [ Element.Pattern(
                            [ (Element.Identifier(ident "a"),
                               [ (Element.Identifier(ident "b"), [ Element.Identifier(ident "c") ]) ])
                              (Element.Identifier(ident "d"),
                               [ (Element.Identifier(ident "e"), [ Element.Identifier(ident "f") ]) ]) ]
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
                      [ Element.Pattern(
                            [ (Element.Identifier(ident "a"),
                               [ (Element.Identifier(ident "b"),
                                  [ Element.Identifier(ident "c"); Element.Identifier(ident "d") ]) ]) ]
                        ) ]
                  ))
                  ""

          testCase "Parse Pattern with all wildcards"
          <| fun _ ->
              let tokens = tokenize "{ $ $ $ }"
              let ast = parse (unsafe tokens)

              Expect.equal
                  ast
                  (Ok(
                      [ Element.Pattern(
                            [ (Element.Slot(Slot.Empty), [ (Element.Slot(Slot.Empty), [ Element.Slot(Slot.Empty) ]) ]) ]
                        ) ]
                  ))
                  ""

          testCase "Parse Pattern with all named wildcards"
          <| fun _ ->
              let tokens = tokenize "{ $entity $attribute $value }"
              let ast = parse (unsafe tokens)

              Expect.equal
                  ast
                  (Ok(
                      [ Element.Pattern(
                            [ (Element.Slot(slot "entity")),
                              [ (Element.Slot(slot "attribute")), [ Element.Slot(slot "value") ] ] ]
                        ) ]
                  ))
                  ""


          testCase "Parse Empty Dataset"
          <| fun _ ->
              let tokens = tokenize "{ }"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Pattern([]) ])) ""

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
              Expect.equal ast (Ok([ Element.Lambda([ "x" ], Element.Name("x")) ])) ""
          testCase "Parse Application"
          <| fun _ ->
              let tokens = tokenize "Bool.and false true"
              let ast = parse (unsafe tokens)

              Expect.equal
                  ast
                  (Ok(
                      [ Element.Application(
                            [ Element.Name("Bool.and"); Element.Bool(false); Element.Bool(true) ]
                        ) ]
                  ))
                  ""
          testCase "Parse Let Statement with Name"
          <| fun _ ->
              let tokens = tokenize "x = 5, x"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Let("x", Element.Int(5I)); Element.Name("x") ])) ""
          testCase "Parse Grouping"
          <| fun _ ->
              let tokens = tokenize "(x = 5, x)"
              let ast = parse (unsafe tokens)

              Expect.equal
                  ast
                  (Ok([ Element.Grouping([ Element.Let("x", Element.Int(5I)); Element.Name("x") ]) ]))
                  ""

          testCase "Parse pipes"
          <| fun _ ->
              let tokens = tokenize "|||"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Application [ Element.Pipe; Element.Pipe; Element.Pipe ] ])) ""
          testCase "Parse question marks"
          <| fun _ ->
              let tokens = tokenize "? ?"
              let ast = parse (unsafe tokens)
              Expect.equal ast (Ok([ Element.Application [ Element.Name("?"); Element.Name ("?") ] ])) ""
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
                  "" ]
