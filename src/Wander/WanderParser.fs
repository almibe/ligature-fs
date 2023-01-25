// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Parser

open Ligature.Wander.Model
open Ligature
open Lexer

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

let nextToken (tokens: WanderToken array) index =
    match tokens[index] with
    | Integer(value) -> Ok((Value(WanderValue.Integer(value)), index + 1))
    | StringLiteral(value) -> Ok((Value(WanderValue.String(value))), index + 1)
    | Boolean(value) -> Ok((Value(WanderValue.Boolean(value))), index + 1)
    | Identifier(value) -> Ok((Value(WanderValue.Identifier(value))), index + 1)
    | e -> error $"Token {e} can not start an expression or statement." None

/// <summary></summary>
/// <param name="tokens">The list of WanderTokens to be parsered.</param>
/// <returns>The AST created from the token list of an Error.</returns>
let parse (tokens: Lexer.WanderToken list) =
    let tokens = List.toArray tokens
    let mutable index = 0
    let mutable ast = []
    let mutable error = None
    while index < (Array.length tokens) && error.IsNone do
        let nextToken = nextToken tokens index
        match nextToken with
        | Error(err) -> error <- Some(err)
        | Ok((token, newIndex)) -> 
            ast <- List.append ast [token]
            index <- newIndex
    match error with
    | None -> Ok(ast)
    | Some(err) -> Error(err)
