// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Fns.Expect

open Ligature.Model
open Wander.Model

let testGroupFn: Fn =
    Fn.Fn(
        { doc = "Make a group of tests."
          examples = []
          args = ""
          result = "" },
        fun _ _ _ arguments ->
            let groupName =
                match arguments.Head with
                | Expression.Element name -> name
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
                            | Some(Expression.Element name),
                              Some(Expression.Term(Term status)),
                              Some(Expression.Element comment) ->
                                let testId =
                                    { value = "test-" + Ulid.Ulid.Ulid.New.ToString()
                                      space = None
                                      langTag = None }

                                [ Assertion.Triple(testId, Term "name", name)
                                  Assertion.Triple(
                                      testId,
                                      Term "state",
                                      { value = status
                                        space = None
                                        langTag = None }
                                  )
                                  Assertion.Triple(testId, Term "comment", comment)
                                  Assertion.Triple(testId, Term "test-group", groupName) ]
                            | _ -> failwith "TODO"
                        | _ -> []) // ignore expressions that don't return test results
                    arguments.Tail

            let testABox: Assertions = Set.ofList testData
            Ok(Expression.Assertions testABox)
    )

let expectEqualFn: Fn =
    Fn.Fn(
        { doc = "Create a test record that to top two values are equal."
          examples = [ "expect-equal(\"is A equal to A?\" A A)" ]
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
                                  Expression.Element
                                      { value = ""
                                        space = None
                                        langTag = None }
                                  Term "status", Expression.Term(Term "pass")
                                  Term "comment",
                                  Expression.Element
                                      { value = ""
                                        space = None
                                        langTag = None } ]
                          children = [] }
                    |> Ok
                else
                    Expression.NodeLiteral
                        { name = Term "Test"
                          attributes =
                            Map.ofList
                                [ Term "name",
                                  Expression.Element
                                      { value = ""
                                        space = None
                                        langTag = None }
                                  Term "status", Expression.Term(Term "fail")
                                  Term "comment",
                                  Expression.Element
                                      { value =
                                          $"assert-equal failed {printExpression first} != {printExpression second}"
                                        space = None
                                        langTag = None } ]
                          children = [] }
                    |> Ok
            | [ Expression.Element name; left; right ] ->
                if left = right then
                    Expression.NodeLiteral
                        { name = Term "Test"
                          attributes =
                            Map.ofList
                                [ Term "name", Expression.Element name
                                  Term "status", Expression.Term(Term "pass")
                                  Term "comment",
                                  Expression.Element
                                      { value = ""
                                        space = None
                                        langTag = None } ]
                          children = [] }
                    |> Ok
                else
                    Expression.NodeLiteral
                        { name = Term "Test"
                          attributes =
                            Map.ofList
                                [ Term "name", Expression.Element name
                                  Term "status", Expression.Term(Term "fail")
                                  Term "comment",
                                  Expression.Element
                                      { value = $"assert-equal failed {printExpression left} != {printExpression right}"
                                        space = None
                                        langTag = None } ]
                          children = [] }
                    |> Ok
            | _ -> error $"expect-equal requires a name and two values." None
    )
