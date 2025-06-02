// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Fns.Expect

open Ligature.Model
open Wander.Interpreter
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
                | Any.Literal name -> name
                | _ -> failwith "Unexpected value."

            let testData =
                List.collect
                    (fun value ->
                        match value with
                        | Any.NodeLiteral testResult ->
                            match
                                testResult.attributes.TryFind(Term "name"),
                                testResult.attributes.TryFind(Term "status"),
                                testResult.attributes.TryFind(Term "comment")
                            with
                            | Some(Any.Literal name), Some(Any.Term status), Some(Any.Literal comment) ->
                                let testId = Term("test-" + Ulid.Ulid.Ulid.New.ToString())

                                [ Assertion.Triple(testId, Term "name", Value.Literal name)
                                  Assertion.Triple(testId, Term "state", Value.Term status)
                                  Assertion.Triple(testId, Term "comment", Value.Literal comment)
                                  Assertion.Triple(testId, Term "test-group", Value.Literal groupName) ]
                            | _ -> failwith "TODO"
                        | _ -> failwith "TODO")
                    arguments.Tail

            let testABox: ABox = Set.ofList testData
            Ok(Any.ABox testABox)
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
                    Any.NodeLiteral
                        { name = Term "Test"
                          attributes =
                            Map.ofList
                                [ Term "name",
                                  Any.Literal
                                      { content = ""
                                        datatype = None
                                        langTag = None }
                                  Term "status", Any.Term(Term "pass")
                                  Term "comment",
                                  Any.Literal
                                      { content = $"Result: {printAny first}"
                                        datatype = None
                                        langTag = None } ]
                          children = [] }
                    |> Ok
                else
                    error $"assert-equal failed {printAny first} != {printAny second}" None
            | [ Any.Literal name; left; right ] ->
                if left = right then
                    Any.NodeLiteral
                        { name = Term "Test"
                          attributes =
                            Map.ofList
                                [ Term "name", Any.Literal name
                                  Term "status", Any.Term(Term "pass")
                                  Term "comment",
                                  Any.Literal
                                      { content = $"Result: {printAny left}"
                                        datatype = None
                                        langTag = None } ]
                          children = [] }
                    |> Ok
                else
                    failwith "TODO"
            | _ -> error $"expect-equal requires a name and two values." None
    )

// let assertFailCommand: Command =
//     { Eval =
//         fun networks local (modules: Modules) (arguments: Arguments) ->
//             match arguments with
//             | [ Any.Tuple tuple ] ->
//                 match evalTuple networks local modules tuple with
//                 | Ok(_) -> error "assert-fail call didn't result in error." None
//                 | Error _ -> Ok((Some(Any.Network Set.empty), networks, local, modules))
//             | args -> error $"assert-fail passed illegal arguments - {args}" None }
