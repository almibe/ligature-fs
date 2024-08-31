// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Assert

open Ligature.Main
open Ligature.Wander.Interpreter

let assertEqualCombinator: Combinator =
    { Name = Name "assert-equal"
      Doc = ""
      Eval =
        fun (combinators: Combinators) (inputState: State) (arguments: Arguments) ->
            match arguments with
            | [ first; second ] ->
                let first =
                    match first with
                    | LigatureValue.Expression e ->
                        match evalExpression combinators inputState e with
                        | Ok(_, Some(res)) -> res
                        | Ok _ -> failwith "Invalid first expression passed to assert-equal."
                        | Error err -> failwith $"Expression errored: {err.UserMessage}."
                    | _ -> first

                let second =
                    match second with
                    | LigatureValue.Expression e ->
                        match evalExpression combinators inputState e with
                        | Ok(_, Some(res)) -> res
                        | Ok _ -> failwith "Invalid second expression passed to assert-equal."
                        | Error err -> failwith $"Expression errored: {err.UserMessage}."
                    | _ -> second

                if first = second then
                    Ok(inputState, Some(LigatureValue.String("Sucess!")))
                else
                    error
                        $"assert-equal failed {Ligature.Wander.Model.prettyPrint first} != {Ligature.Wander.Model.prettyPrint second}"
                        None
            | args -> error $"assert-equal passed illegal arguments - {args}" None }

let assertCombinators =
    Map.ofList
        [ (assertEqualCombinator.Name, (assertEqualCombinator)) ]
