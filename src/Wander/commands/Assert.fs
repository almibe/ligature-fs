// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Commands.Assert

open Ligature.Main
open Wander.Interpreter
open Wander.Model

let assertEqualCommand: Command =
    { Name = Symbol "assert-equal"
      Doc = "Check that two values are equal."
      Eval =
        fun (commands: Commands) networks (arguments: Arguments) ->
            match arguments with
            | [ first; second ] ->
                let first =
                    match first with
                    | WanderValue.Call(n, e) ->
                        match evalCall commands networks (n, e) with
                        | Ok(Some(res)) -> res
                        | Ok _ -> failwith "Invalid first expression passed to assert-equal."
                        | Error err -> failwith $"Expression errored: {err.UserMessage}."
                    | _ -> first

                let second =
                    match second with
                    | WanderValue.Call(n, e) ->
                        match evalCall commands networks (n, e) with
                        | Ok(Some(res)) -> res
                        | Ok _ -> failwith "Invalid first expression passed to assert-equal."
                        | Error err -> failwith $"Expression errored: {err.UserMessage}."
                    | _ -> second

                if first = second then
                    Ok(Some(WanderValue.Symbol(Symbol("Sucess!"))))
                else
                    error $"assert-equal failed {prettyPrint first} != {prettyPrint second}" None
            | args -> error $"assert-equal passed illegal arguments - {args}" None }

let assertCommands = Map.ofList [ (assertEqualCommand.Name, (assertEqualCommand)) ]
