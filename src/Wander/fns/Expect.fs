// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Fns.Expect

open Ligature.Model
open Wander.Model

let testGroupFn: Fn =
    Fn(
        { doc = "Make a group of tests."
          examples = []
          args = ""
          result = "" },
        fun _ _ _ arguments ->
            let groupName =
                match arguments.Head with
                | Expression.Literal name -> name
                | _ -> failwith "Unexpected value."

            let testData =
                List.collect
                    (fun value ->
                        match value with
                        | Expression.NodeLiteral testResult ->
                            match
                                testResult.attributes.TryFind(Term "name"),
                                testResult.attributes.TryFind(Term "status"),
                                testResult.attributes.TryFind(Term "comment")
                            with
                            | Some(Expression.Literal name),
                              Some(Expression.Term status),
                              Some(Expression.Literal comment) ->
                                let testId = Term("test-" + Ulid.Ulid.Ulid.New.ToString())

                                [ Assertion.Triple(testId, Term "name", Value.Literal name)
                                  Assertion.Triple(testId, Term "state", Value.Term status)
                                  Assertion.Triple(testId, Term "comment", Value.Literal comment)
                                  Assertion.Triple(testId, Term "test-group", Value.Literal groupName) ]
                            | _ -> failwith "TODO"
                        | _ -> failwith "TODO")
                    arguments.Tail

            let testABox: ABox = Set.ofList testData
            Ok(Expression.ABox testABox)
    )

let expectEqualFn: Fn =
    Fn(
        { doc = "Create a test record that to top two values are equal."
          examples = [ "(expect-equal \"is A equal to A?\" A A)" ]
          args = "Literal Any Any"
          result = "Record" },
        fun _ _ _ arguments ->
            match arguments with
            | [ first; second ] ->
                if first = second then
                    Expression.NodeLiteral
                        { name = Term "Test"
                          attributes =
                            Map.ofList
                                [ Term "name",
                                  Expression.Literal
                                      { content = ""
                                        datatype = None
                                        langTag = None }
                                  Term "status", Expression.Term(Term "pass")
                                  Term "comment",
                                  Expression.Literal
                                      { content = ""
                                        datatype = None
                                        langTag = None } ]
                          children = [] }
                    |> Ok
                else
                    error $"assert-equal failed {printAny first} != {printAny second}" None
            | [ Expression.Literal name; left; right ] ->
                if left = right then
                    Expression.NodeLiteral
                        { name = Term "Test"
                          attributes =
                            Map.ofList
                                [ Term "name", Expression.Literal name
                                  Term "status", Expression.Term(Term "pass")
                                  Term "comment",
                                  Expression.Literal
                                      { content = ""
                                        datatype = None
                                        langTag = None } ]
                          children = [] }
                    |> Ok
                else
                    Expression.NodeLiteral
                        { name = Term "Test"
                          attributes =
                            Map.ofList
                                [ Term "name", Expression.Literal name
                                  Term "status", Expression.Term(Term "fail")
                                  Term "comment",
                                  Expression.Literal
                                      { content = $"{name} assert-equal failed {printAny left} != {printAny right}"
                                        datatype = None
                                        langTag = None } ]
                          children = [] }
                    |> Ok
            | _ -> error $"expect-equal requires a name and two values." None
    )
