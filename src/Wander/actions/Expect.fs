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
            match arguments.Head with
            | Any.Literal(Literal(name, Term "")) -> printfn $"Running Test Group: {name}"
            | _ -> failwith "Unexpected value."

            List.iter
                (fun arg ->
                    match arg with
                    | Any.Record record ->
                        match
                            record.TryFind(Any.Literal(Literal("name", Term ""))),
                            record.TryFind(Any.Literal(Literal("expect", Term ""))),
                            record.TryFind(Any.Literal(Literal("left", Term ""))),
                            record.TryFind(Any.Literal(Literal("right", Term "")))
                        with
                        | Some(Any.Literal(Literal(name, Term ""))),
                          Some(Any.Literal(Literal("=", Term ""))),
                          Some left,
                          Some right ->
                            printfn $"  Starting test {name}"

                            if left = right then
                                printfn "   - Passed."
                            else
                                printfn "   X - Failed"
                                printfn $"  {left} != {right}"
                        | _ -> failwith "TODO"
                    | x -> failwith $"Unexpected value, {x}.")
                arguments.Tail

            Ok(Any.Record Map.empty)
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
                // let first =
                //     match first with
                //     | Any.Tuple tuple ->
                //         match evalTuple networks local modules variables tuple with
                //         | Ok((Some(res), _, _, _, _)) -> res
                //         | Ok _ -> failwith "Invalid first expression passed to assert-equal."
                //         | Error err -> failwith $"Expression errored: {err.UserMessage}."
                //     | Any.Slot variable ->
                //         match Map.tryFind variable variables with
                //         | Some(res) -> res
                //         | None -> failwith "Invalid first expression passed to assert-equal."
                //     | _ -> first

                // let second =
                //     match second with
                //     | Any.Tuple tuple ->
                //         match evalTuple networks local modules variables tuple with
                //         | Ok((Some(res), _, _, _, _)) -> res
                //         | Ok _ -> failwith "Invalid second expression passed to assert-equal."
                //         | Error err -> failwith $"Expression errored: {err.UserMessage}."
                //     | Any.Slot variable ->
                //         match Map.tryFind variable variables with
                //         | Some(res) -> res
                //         | None -> failwith "Invalid second expression passed to assert-equal."
                //     | _ -> second

                if first = second then
                    Ok(Any.Assertions Set.empty)
                else
                    error $"assert-equal failed {printAny first} != {printAny second}" None
            | [ Any.Literal name; left; right ] ->
                // Any.Record(Map.empty)
                // if first = second then
                //     Ok(Any.Assertions Set.empty)
                // else
                //     error $"assert-equal failed {printAny first} != {printAny second}" None
                Any.Record(
                    Map.ofList
                        [ Any.Literal(Literal("name", Term "")), Any.Literal name
                          Any.Literal(Literal("expect", Term "")), Any.Literal(Literal("=", Term ""))
                          Any.Literal(Literal("left", Term "")), left
                          Any.Literal(Literal("right", Term "")), right ]
                )
                |> Ok
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
