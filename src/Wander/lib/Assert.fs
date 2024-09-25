// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Assert

open Ligature.Main
open Ligature.Wander.Interpreter

let assertEqualCombinator: Combinator =
    { Name = Symbol "assert-equal"
      Doc = "Check that two values are equal."
      Signature = [ LigatureType.Any; LigatureType.Any ], None
      Eval =
        fun (combinators: Combinators) networks (arguments: Arguments) ->
            match arguments with
            | [ first; second ] ->
                let first =
                    match first with
                    | WanderValue.Expression e ->
                        match evalExpression combinators networks e with
                        | Ok(Some(res)) -> res
                        | Ok _ -> failwith "Invalid first expression passed to assert-equal."
                        | Error err -> failwith $"Expression errored: {err.UserMessage}."
                    | _ -> first

                let second =
                    match second with
                    | WanderValue.Expression e ->
                        match evalExpression combinators networks e with
                        | Ok(Some(res)) -> res
                        | Ok _ -> failwith "Invalid first expression passed to assert-equal."
                        | Error err -> failwith $"Expression errored: {err.UserMessage}."
                    | _ -> second

                if first = second then
                    Ok(Some(WanderValue.Symbol(Symbol("Sucess!"))))
                else
                    error
                        $"assert-equal failed {Ligature.Wander.Model.prettyPrint first} != {Ligature.Wander.Model.prettyPrint second}"
                        None
            | args -> error $"assert-equal passed illegal arguments - {args}" None }

let assertCombinators =
    Map.ofList [ (assertEqualCombinator.Name, (assertEqualCombinator)) ]
