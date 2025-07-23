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
                List.fold
                    (fun state value ->
                        match value with
                        | Expression.Assertions testResult -> Set.union state testResult
                        | _ -> failwith "TODO") // ignore expressions that don't return test results
                    Set.empty
                    application.arguments.Tail

            let testABox: Assertions = testData
            Ok(Expression.Assertions testABox)
    )

let expectEqualFn: Fn =
    Fn.Fn(
        { doc = "Create a test record that to top two values are equal."
          examples = [ "expect-equal(\"is A equal to A?\" A A)" ]
          args = "Literal Any Any"
          result = "Assertions" },
        fun _ _ application ->
            match application.arguments with
            | [ first; second ] ->
                if first = second then
                    let testId =
                        { value = Term $"test-{Ulid.Ulid.Ulid.New}"
                          space = None
                          langTag = None }

                    Expression.Assertions(
                        Set.ofList
                            [ Assertion.Triple(testId, Term "name", el "Unnamed")
                              Assertion.Triple(testId, Term "state", el "pass")
                              Assertion.Triple(testId, Term "comment", el "")
                              //Assertion.Triple(testId, Term "test-group", groupName)
                              ]
                    )
                    |> Ok
                else
                    let testId =
                        { value = Term $"test-{Ulid.Ulid.Ulid.New}"
                          space = None
                          langTag = None }

                    Expression.Assertions(
                        Set.ofList
                            [ Assertion.Triple(testId, Term "name", el "Unnamed")
                              Assertion.Triple(testId, Term "state", el "fail")
                              Assertion.Triple(
                                  testId,
                                  Term "comment",
                                  el $"assert-equal failed {printExpression first} != {printExpression second}"
                              )
                              //Assertion.Triple(testId, Term "test-group", groupName)
                              ]
                    )
                    |> Ok
            | [ Expression.Element name; left; right ] ->
                if left = right then
                    let testId =
                        { value = Term $"test-{Ulid.Ulid.Ulid.New}"
                          space = None
                          langTag = None }

                    Expression.Assertions(
                        Set.ofList
                            [ Assertion.Triple(testId, Term "name", name)
                              Assertion.Triple(testId, Term "state", el "pass")
                              Assertion.Triple(testId, Term "comment", el "")
                              //Assertion.Triple(testId, Term "test-group", groupName)
                              ]
                    )
                    |> Ok
                else
                    failwith "TODO"
            // Expression.ObjectView
            //     { root = el "Test"
            //       links =
            //         Map.ofList
            //             [ Term "name", [ emptyObjectView name ]
            //               Term "status", [ emptyObjectView (el "fail") ]
            //               Term "comment",
            //               [ emptyObjectView (
            //                     el $"assert-equal failed {printExpression left} != {printExpression right}"
            //                 ) ] ]
            //       concepts = Set.empty }
            // |> Ok
            | _ -> error $"expect-equal requires a name and two values." None
    )
