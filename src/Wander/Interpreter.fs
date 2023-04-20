// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Interpreter

open Ligature
open Ligature.Wander.Model

let interpret (elements: Parser.Element list) (bindings: Bindings): Result<WanderValue, LigatureError> =
    failwith "todo"

// let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

// let evalName name bindings =
//     match read name bindings with
//     | None -> todo
//     | Some(value) -> Ok((value, bindings))

// let rec evalExpression bindings expression =
//     let rec bindArguments (args: Ligature.Wander.Model.Expression list) (parameters: string list) (bindings: Bindings): Result<Bindings, LigatureError> =
//         if List.length args <> List.length parameters then
//             todo
//         else if List.isEmpty args && List.isEmpty parameters then
//             Ok bindings
//         else
//             let arg = List.head args
//             let parameter = List.head parameters
//             let value = evalExpression bindings arg
//             match value with
//             | Ok (value, _) ->
//                 Ok(bind parameter value bindings)
//             | Error(err) -> Error(err)

//     match expression with
//     | Expression.Value(value) -> Ok((value, bindings))
//     | Expression.Name(name) -> evalName name bindings
//     | Expression.Scope(expressions) ->
//         let bindings' = addScope bindings
//         match evalExpressions bindings' expressions with
//         | Error(err) -> Error(err)
//         | Ok((res, _)) -> Ok((res, bindings))
//     | Expression.LetStatement(name, expression) ->
//         let res = evalExpression bindings expression

//         match res with
//         | Ok((value, _)) ->
//             let bindings = bind name value bindings
//             Ok((WanderValue.Nothing, bindings))
//         | Error(_) -> res
//     | Expression.FunctionCall(name, args) ->
//         let args = List.map ( fun a -> 
//             match evalExpression bindings a with
//             | Ok(v, _) -> Expression.Value(v)
//             | Error(err) -> todo)
//                     args

//         match Bindings.read name bindings with
//         | Some(WanderValue.NativeFunction(funct)) -> 
//             match funct.Run args bindings with
//             | Ok res -> Ok (res, bindings)
//             | Error(err) -> Error(err)
//         | Some(WanderValue.Lambda(parameters, body)) ->
//             match bindArguments args parameters bindings with
//             | Ok(bindings) -> evalExpressions bindings body
//             | Error(err) -> Error(err)
//         | None -> error $"{name} function not found." None
//         | _ -> todo //type error
//     | Expression.Conditional(conditional) ->
//         let ifCondition = evalExpression bindings conditional.ifCase.condition
//         let mutable result = None

//         result <-
//             match ifCondition with
//             | Ok(WanderValue.Boolean(true), bindings) -> Some (evalExpression bindings conditional.ifCase.body)
//             | Ok(WanderValue.Boolean(false), _) -> None
//             | Ok _ -> Some(error "Type mismatch, expecting boolean." None)
//             | Error x -> Some(Error x)

//         let mutable elsifCases = conditional.elsifCases
//         while Option.isNone result && not elsifCases.IsEmpty do
//             let case = elsifCases.Head
//             result <- 
//                 match evalExpression bindings case.condition with
//                 | Ok(WanderValue.Boolean(true), bindings) -> Some (evalExpression bindings case.body)
//                 | Ok(WanderValue.Boolean(false), _) -> None
//                 | Ok _ -> Some(error "Type mismatch, expecting boolean." None)
//                 | Error x -> Some(Error x)
//             elsifCases <- elsifCases.Tail

//         match result with
//         | None -> evalExpression bindings conditional.elseBody
//         | Some(result) -> result
//     | Expression.TupleExpression(expressions) ->
//         let mutable error = None
//         let res: WanderValue list = 
//             //TODO this doesn't short circuit on first error
//             List.map (fun e ->
//                 match evalExpression bindings e with
//                 | Ok(value, _) -> value
//                 | Error(err) -> 
//                     if Option.isNone error then error <- Some(err)
//                     WanderValue.Nothing
//                         ) expressions
//         match error with
//         | None -> Ok((WanderValue.Tuple(res), bindings))
//         | Some(err) -> Error(err)

// and evalExpressions
//     (bindings: Bindings.Bindings<_, _>)
//     (expressions: Expression list)
//     : Result<(WanderValue * Bindings.Bindings<_, _>), LigatureError> =
//     match List.length expressions with
//     | 0 -> Ok(WanderValue.Nothing, bindings)
//     | 1 -> evalExpression bindings (List.head expressions)
//     | _ ->
//         let mutable result = Ok(WanderValue.Nothing, bindings)
//         let mutable cont = true
//         let mutable bindings = bindings
//         let mutable expressions = expressions
//         while cont && not (List.isEmpty expressions) do
//             result <- evalExpression bindings (List.head expressions)
//             expressions <- List.tail expressions
//             match result with
//             | Ok((res, b)) ->
//                 bindings <- b
//                 result <- Ok((res, b))
//             | Error(err) ->
//                 result <- Error(err)
//                 cont <- false
//         result

// let rec eval (bindings: Bindings.Bindings<_, _>) (expressions: Expression list) =
//     match expressions with
//     | [] -> Ok(WanderValue.Nothing)
//     | _ ->
//         let evalResult = evalExpression bindings (List.head expressions)

//         match (evalResult, (List.tail expressions)) with
//         | (Ok((value, _)), []) -> Ok(value)
//         | (Ok(_, bindings), tail) -> eval bindings tail
//         | (Error(error), _) -> Error(error)

// let interpret (ast: Expression list) bindings =
//     eval bindings ast
