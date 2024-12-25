// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Commands.Assert

open Ligature.Model
open Wander.Interpreter
open Wander.Model

let assertEqualCommand: Command =
    { Eval =
        fun local (modules: Modules) variables (arguments: Arguments) ->
            match arguments with
            | [ first; second ] ->
                let first =
                    match first with
                    | Any.Quote quote ->
                        match evalQuote local modules variables quote with
                        | Ok((Some(res), _, _, _)) -> res
                        | Ok _ -> failwith "Invalid first expression passed to assert-equal."
                        | Error err -> failwith $"Expression errored: {err.UserMessage}."
                    | Any.Variable variable ->
                        match Map.tryFind variable variables with
                        | Some(res) -> res
                        | None -> failwith "Invalid first expression passed to assert-equal."
                    | _ -> first

                let second =
                    match second with
                    | Any.Quote quote ->
                        match evalQuote local modules variables quote with
                        | Ok((Some(res), _, _, _)) -> res
                        | Ok _ -> failwith "Invalid second expression passed to assert-equal."
                        | Error err -> failwith $"Expression errored: {err.UserMessage}."
                    | Any.Variable variable ->
                        match Map.tryFind variable variables with
                        | Some(res) -> res
                        | None -> failwith "Invalid second expression passed to assert-equal."
                    | _ -> second

                if first = second then
                    Ok((Some(Any.Element(Element "Sucess!")), local, modules, variables))
                else
                    error $"assert-equal failed {prettyPrint first} != {prettyPrint second}" None
            | args -> error $"assert-equal passed illegal arguments - {args}" None }

let assertFailCommand: Command =
    { Eval =
        fun local (modules: Modules) variables (arguments: Arguments) ->
            match arguments with
            | [ Any.Quote quote ] ->
                match evalQuote local modules variables quote with
                | Ok(_) -> error "assert-fail call didn't result in error." None
                | Error _ -> Ok((Some(Any.Network Set.empty), local, modules, variables))
            | args -> error $"assert-fail passed illegal arguments - {args}" None }

let assertCommands =
    Map.ofList
        [ (Element "assert-equal", (assertEqualCommand))
          (Element "assert-fail", (assertFailCommand)) ]
