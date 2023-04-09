// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Interpreter

open Ligature
open Ligature.Wander.Model
open Ligature.Wander.Bindings

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

let evalName name bindings =
    match Bindings.read name bindings with
    | None -> todo
    | Some(value) -> Ok((value, bindings))

let rec evalExpression bindings expression =
    let rec bindArguments (args: Ligature.Wander.Model.Expression list) (parameters: string list) (bindings: Bindings): Result<Bindings, LigatureError> =
        if List.length args <> List.length parameters then
            todo
        else if List.isEmpty args && List.isEmpty parameters then
            Ok bindings
        else
            let arg = List.head args
            let parameter = List.head parameters
            let value = evalExpression bindings arg
            match value with
            | Ok (value, _) ->
                Ok(Bindings.bind parameter value bindings)
            | Error(err) -> Error(err)

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
    | FunctionCall(name, args) ->
        let args = List.map ( fun a -> 
            match evalExpression bindings a with
            | Ok(v, _) -> Value(v)
            | Error(err) -> todo)
                    args

        match Bindings.read name bindings with
        | Some(NativeFunction(funct)) -> 
            match funct.Run args with
            | Ok res -> Ok (res, bindings)
            | Error(err) -> Error(err)
        | Some(Lambda(parameters, body)) ->
            match bindArguments args parameters bindings with
            | Ok(bindings) -> evalExpressions bindings body
            | Error(err) -> Error(err)
        //TODO add check for Lambda
        | None -> todo //not found
        | _ -> todo //type error
        //TODO look up name in bindings
        //TODO check if looked up value exists/is a NativeFunction/Lambda
        //TODO call method on NativeFunction/Lambda
        //TODO return result
    | Conditional(conditional) ->
        let ifCondition = evalExpression bindings conditional.ifCase.condition
        let mutable result = None

        result <-
            match ifCondition with
            | Ok(Boolean(true), bindings) -> Some (evalExpression bindings conditional.ifCase.body)
            | Ok(Boolean(false), _) -> None
            | Ok _ -> Some(error "Type mismatch, expecting boolean." None)
            | Error x -> Some(Error x)

        let mutable elsifCases = conditional.elsifCases
        while Option.isNone result && not elsifCases.IsEmpty do
            let case = elsifCases.Head
            result <- 
                match evalExpression bindings case.condition with
                | Ok(Boolean(true), bindings) -> Some (evalExpression bindings case.body)
                | Ok(Boolean(false), _) -> None
                | Ok _ -> Some(error "Type mismatch, expecting boolean." None)
                | Error x -> Some(Error x)
            elsifCases <- elsifCases.Tail

        match result with
        | None -> evalExpression bindings conditional.elseBody
        | Some(result) -> result
    | _ -> error $"Could not eval {expression}" None

and evalExpressions
    (bindings: Bindings.Bindings<_, _>)
    (expressions: Expression list)
    : Result<(WanderValue * Bindings.Bindings<_, _>), LigatureError> =
    match List.length expressions with
    | 0 -> Ok(Nothing, bindings)
    | 1 -> evalExpression bindings (List.head expressions)
    | _ ->
        let mutable result = Ok(Nothing, bindings)
        let mutable cont = true

        while cont do
            result <- evalExpression bindings (List.head expressions)

            match result with
            | Ok(res) -> result <- Ok(res)
            | Error(err) ->
                result <- Error(err)
                cont <- false

        result

let rec eval (bindings: Bindings.Bindings<_, _>) (expressions: Expression list) =
    match expressions with
    | [] -> Ok(Nothing)
    | _ ->
        let evalResult = evalExpression bindings (List.head expressions)

        match (evalResult, (List.tail expressions)) with
        | (Ok((value, _)), []) -> Ok(value)
        | (Ok(_, bindings), tail) -> eval bindings tail
        | (Error(error), _) -> Error(error)

let interpret (ast: Expression list) =
    let bindings = 
        Bindings.newBindings ()
        |> Preludes.bindStandardLibrary
    eval bindings ast
