// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Parser

open Ligature.Wander.Model
open Ligature
open Lexer

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

let integerNibbler =
    Nibblers.takeCond (fun token ->
        match token with
        | Integer(_) -> true
        | _ -> false)

let stringNibbler =
    Nibblers.takeCond (fun token ->
        match token with
        | StringLiteral(_) -> true
        | _ -> false)

let booleanNibbler =
    Nibblers.takeCond (fun token ->
        match token with
        | Boolean(_) -> true
        | _ -> false)

let identifierNibbler =
    Nibblers.takeCond (fun token ->
        match token with
        | Identifier(_) -> true
        | _ -> false)

let simpleValueNibbler = (Nibblers.takeFirst [ integerNibbler; stringNibbler; booleanNibbler; identifierNibbler ])

let valueNibbler =
    Gaze.map simpleValueNibbler (fun token ->
        match token with
        | Integer(i) -> Value(WanderValue.Integer(i))
        | StringLiteral(s) -> Value(WanderValue.String(s))
        | Boolean(b) -> Value(WanderValue.Boolean(b))
        | Identifier(i) -> Value(WanderValue.Identifier(i))
        | _ -> todo) //error $"Token {e} can not start an expression or statement." None)

let nameNibbler =
    Nibblers.takeCond (fun token ->
        match token with
        | Name(_) -> true
        | _ -> false)

let nameExpressionNibbler =
    Gaze.map nameNibbler (fun name ->
        match name with
        | Name(name) -> Expression.Name(name)
        | _ -> todo)

let letStatementNibbler =
    Gaze.map
        (Nibblers.takeAll
            [ Nibblers.take LetKeyword
              nameNibbler
              Nibblers.take EqualSign
              simpleValueNibbler ])
        (fun tokens ->
            match tokens with
            | [ _; Name(name); _; Integer(i) ] -> LetStatement(name, Value(WanderValue.Integer(i)))
            | [ _; Name(name); _; Boolean(b) ] -> LetStatement(name, Value(WanderValue.Boolean(b)))
            | [ _; Name(name); _; StringLiteral(s)  ] -> LetStatement(name, Value(WanderValue.String(s)))
            | [ _; Name(name); _; Identifier(i)] -> LetStatement(name, Value(WanderValue.Identifier(i)))
            | _ -> todo)

let expressionNibbler =
    Nibblers.repeat (Nibblers.takeFirst [ valueNibbler; letStatementNibbler; nameExpressionNibbler ])

/// <summary></summary>
/// <param name="tokens">The list of WanderTokens to be parsered.</param>
/// <returns>The AST created from the token list of an Error.</returns>
let parse (tokens: Lexer.WanderToken list) =
    let tokens =
        List.filter
            (fun token ->
                match token with
                | Comment(_)
                | WhiteSpace(_)
                | NewLine(_) -> false
                | _ -> true)
            tokens

    if tokens.IsEmpty then
        Ok([])
    else    
        let gaze = Gaze.fromList tokens
        let res = Gaze.attempt expressionNibbler gaze

        match res with
        | Some(ast) -> Ok(ast)
        | None -> error $"No match {tokens}" None

let parseString (input: string) =
    match tokenize input with
    | Ok(tokens) -> parse tokens
    | Error(err) -> Error(err)
