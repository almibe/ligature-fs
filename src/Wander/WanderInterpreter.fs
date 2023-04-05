// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Interpreter

open Ligature
open Ligature.Wander.Model

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

let evalName name bindings =
    match Bindings.read name bindings with
    | None -> todo
    | Some(value) -> Ok((value, bindings))

let rec evalExpression bindings expression =
    match expression with
    | Value(value) -> Ok((value, bindings))
    | Name(name) -> evalName name bindings
    | Scope(expressions) -> evalExpressions bindings expressions
    | LetStatement(name, expression) ->
        let res = evalExpression bindings expression

        match res with
        | Ok((value, _)) ->
            let bindings = Bindings.bind name value bindings
            Ok((Nothing, bindings))
        | Error(_) -> res
    | _ -> error $"Could not eval {expression}" None

and evalExpressions (bindings: Bindings.Bindings) (expressions: Expression list): Result<(WanderValue * Bindings.Bindings), LigatureError> =
    match List.length expressions with
    | 0 -> Ok (Nothing, bindings)
    | 1 -> evalExpression bindings (List.head expressions)
    | _ ->
        let mutable result = Ok (Nothing, bindings)
        let mutable cont = true
        while cont do
            result <- evalExpression bindings (List.head expressions)
            match result with
            | Ok(res) -> result <- Ok(res)
            | Error(err) ->
                result <- Error(err)
                cont <- false
        result

let rec eval (bindings: Bindings.Bindings) (expressions: Expression list) =
    match expressions with
    | [] -> Ok(Nothing)
    | _ ->
        let evalResult = evalExpression bindings (List.head expressions)

        match (evalResult, (List.tail expressions)) with
        | (Ok((value, _)), []) -> Ok(value)
        | (Ok(_, bindings), tail) -> eval bindings tail
        | (Error(error), _) -> Error(error)

let interpret (ast: Expression list) =
    let bindings = Bindings.newBindings ()
    eval bindings ast
