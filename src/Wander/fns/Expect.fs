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
        fun _ _ application ->
            let groupName =
                match application.arguments.Head with
                | Expression.Element name -> name
                | _ -> failwith "Unexpected value."

            let testData =
                List.collect
                    (fun value ->
                        match value with
                        | Expression.ObjectView testResult ->
                            match
                                testResult.links.TryFind(Term "name"),
                                testResult.links.TryFind(Term "status"),
                                testResult.links.TryFind(Term "comment")
                            with
                            | Some [ { root = name } ], Some [ { root = status } ], Some [ { root = comment } ] ->

                                let testId =
                                    { value = Term $"test-{Ulid.Ulid.Ulid.New}"
                                      space = None
                                      langTag = None }

                                [ Assertion.Triple(testId, Term "name", name)
                                  Assertion.Triple(testId, Term "state", status)
                                  Assertion.Triple(testId, Term "comment", comment)
                                  Assertion.Triple(testId, Term "test-group", groupName) ]
                            | _ -> failwith "TODO"
                        | _ -> []) // ignore expressions that don't return test results
                    application.arguments.Tail

            let testABox: Assertions = Set.ofList testData
            Ok(Expression.Assertions testABox)
    )

let expectEqualFn: Fn =
    Fn.Fn(
        { doc = "Create a test record that to top two values are equal."
          examples = [ "expect-equal(\"is A equal to A?\" A A)" ]
          args = "Literal Any Any"
          result = "Record" },
        fun _ _ application ->
            match application.arguments with
            | [ first; second ] ->
                if first = second then
                    Expression.ObjectView
                        { root = el "Test"
                          links =
                            Map.ofList
                                [ Term "name", [ emptyObjectView (el "") ]
                                  Term "status", [ emptyObjectView (el "pass") ]
                                  Term "comment", [ emptyObjectView (el "") ] ]
                          concepts = Set.empty }
                    |> Ok
                else
                    Expression.ObjectView
                        { root = el "Test"
                          links =
                            Map.ofList
                                [ Term "name", [ emptyObjectView (el "") ]
                                  Term "status", [ emptyObjectView (el "fail") ]
                                  Term "comment",

                                  [ emptyObjectView (
                                        el $"assert-equal failed {printExpression first} != {printExpression second}"
                                    ) ] ]
                          concepts = Set.empty }
                    |> Ok
            | [ Expression.Element name; left; right ] ->
                if left = right then
                    Expression.ObjectView
                        { root = el "Test"
                          links =
                            Map.ofList
                                [ Term "name", [ emptyObjectView name ]
                                  Term "status", [ emptyObjectView (el "pass") ]
                                  Term "comment", [ emptyObjectView (el "") ] ]
                          concepts = Set.empty }
                    |> Ok
                else
                    failwith "TODO"
            // Expression.NodeLiteral
            //     { name = Term "Test"
            //       attributes =
            //         Map.ofList
            //             [ Term "name", Expression.Element name
            //               Term "status", Expression.Term(Term "fail")
            //               Term "comment",
            //               Expression.Element
            //                   { value = $"assert-equal failed {printExpression left} != {printExpression right}"
            //                     space = None
            //                     langTag = None } ]
            //       children = [] }
            // |> Ok
            | _ -> error $"expect-equal requires a name and two values." None
    )
