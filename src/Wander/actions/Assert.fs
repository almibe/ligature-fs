// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Actions.Assert

open Ligature.Model
open Wander.Interpreter
open Wander.Model

let assertEqualAction: Action =
    Action.Stack(fun stack ->
        match stack with
        | first :: second :: tail ->
            // let first =
            //     match first with
            //     | Any.Quote quote ->
            //         match evalQuote networks local modules variables quote with
            //         | Ok((Some(res), _, _, _, _)) -> res
            //         | Ok _ -> failwith "Invalid first expression passed to assert-equal."
            //         | Error err -> failwith $"Expression errored: {err.UserMessage}."
            //     | Any.Variable variable ->
            //         match Map.tryFind variable variables with
            //         | Some(res) -> res
            //         | None -> failwith "Invalid first expression passed to assert-equal."
            //     | _ -> first

            // let second =
            //     match second with
            //     | Any.Quote quote ->
            //         match evalQuote networks local modules variables quote with
            //         | Ok((Some(res), _, _, _, _)) -> res
            //         | Ok _ -> failwith "Invalid second expression passed to assert-equal."
            //         | Error err -> failwith $"Expression errored: {err.UserMessage}."
            //     | Any.Variable variable ->
            //         match Map.tryFind variable variables with
            //         | Some(res) -> res
            //         | None -> failwith "Invalid second expression passed to assert-equal."
            //     | _ -> second

            if first = second then
                Ok(tail)
            else
                error $"assert-equal failed {prettyPrint first} != {prettyPrint second}" None
        | _ -> error $"assert-equal requires two values on stack." None)

// let assertFailCommand: Command =
//     { Eval =
//         fun networks local (modules: Modules) (arguments: Arguments) ->
//             match arguments with
//             | [ Any.Quote quote ] ->
//                 match evalQuote networks local modules quote with
//                 | Ok(_) -> error "assert-fail call didn't result in error." None
//                 | Error _ -> Ok((Some(Any.Network Set.empty), networks, local, modules))
//             | args -> error $"assert-fail passed illegal arguments - {args}" None }

let assertCommands = Map.empty
// Map.ofList
//     [ (Element "assert-equal", (assertEqualCommand))
//       (Element "assert-fail", (assertFailCommand)) ]
