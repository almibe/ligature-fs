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
            let name =
                match arguments.Head with
                | Any.Literal { content = name } -> $"Running Test Group: {name}"
                | _ -> failwith "Unexpected value."

            Ok(Any.Tuple arguments.Tail)
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
                                [ Term "name", Any.Literal { content = ""; datatype = None; langTag = None }
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
