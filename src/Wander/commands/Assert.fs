// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Commands.Assert

open Ligature.Main
open Wander.Interpreter
open Wander.Model

let assertEqualCommand: Command =
    { Name = Element "assert-equal"
      Doc = "Check that two values are equal."
      Eval =
        fun (commands: Commands) networks (arguments: Arguments) ->
            match arguments with
            | [ first; second ] ->
                let first =
                    match first with
                    | Any.Quote quote ->
                        match evalQuote commands networks quote with
                        | Ok(Some(res)) -> res
                        | Ok _ -> failwith "Invalid first expression passed to assert-equal."
                        | Error err -> failwith $"Expression errored: {err.UserMessage}."
                    | _ -> first

                let second =
                    match second with
                    | Any.Quote quote ->
                        match evalQuote commands networks quote with
                        | Ok(Some(res)) -> res
                        | Ok _ -> failwith "Invalid first expression passed to assert-equal."
                        | Error err -> failwith $"Expression errored: {err.UserMessage}."
                    | _ -> second

                if first = second then
                    Ok(Some(Any.Element(Element "Sucess!")))
                else
                    error $"assert-equal failed {prettyPrint first} != {prettyPrint second}" None
            | args -> error $"assert-equal passed illegal arguments - {args}" None }

let assertFailCommand: Command =
    { Name = Element "assert-fail"
      Doc = "Check that a call results in an error."
      Eval =
        fun (commands: Commands) networks (arguments: Arguments) ->
            match arguments with
            | [ Any.Quote quote ] ->
                match evalQuote commands networks quote with
                | Ok(_) -> error "assert-fail call didn't result in error." None
                | Error _ -> Ok(Some(Any.Network Set.empty))
            | args -> error $"assert-fail passed illegal arguments - {args}" None }

let assertCommands =
    Map.ofList
        [ (assertEqualCommand.Name, (assertEqualCommand))
          (assertFailCommand.Name, (assertFailCommand)) ]
