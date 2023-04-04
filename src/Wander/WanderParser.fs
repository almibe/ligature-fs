// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module rec Ligature.Wander.Parser

open Ligature.Wander.Model
open Ligature
open Lexer
open FsToolkit.ErrorHandling

let todo<'T> : 'T = raise (System.NotImplementedException("todo"))

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

let scopeNibbler () =
    Nibblers.between OpenBrace expressionNibbler CloseBrace

let scopeExpressionNibbler =
    Gaze.map (scopeNibbler ()) (fun expr ->
        Scope [])

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
    Nibblers.repeat (Nibblers.takeFirst [
        scopeExpressionNibbler
        valueNibbler
        letStatementNibbler
        nameExpressionNibbler
        ])

// /// <summary></summary>
// /// <param name="tokens">The list of WanderTokens to be parsered.</param>
// /// <returns>The AST created from the token list of an Error.</returns>
// let parse (tokens: Lexer.WanderToken list) =
//     let tokens =
//         List.filter
//             (fun token ->
//                 match token with
//                 | Comment(_)
//                 | WhiteSpace(_)
//                 | NewLine(_) -> false
//                 | _ -> true)
//             tokens

//     if tokens.IsEmpty then
//         Ok([])
//     else    
//         let gaze = Gaze.fromList tokens
//         let res = Gaze.attempt expressionNibbler gaze

//         match res with
//         | Some(ast) -> Ok(ast)
//         | None -> error $"No match {tokens}" None

// let parseString (input: string) =
//     match tokenize input with
//     | Ok(tokens) -> parse tokens
//     | Error(err) -> Error(err)

let readNextElement (gaze: Gaze.Gaze<WanderToken>): Expression option =
    todo

let readLetStatement gaze =
    Gaze.mark gaze
    // result {

    // }
    if Gaze.next gaze = Ok(LetKeyword) then
        todo

    todo

let readInteger (gaze: Gaze.Gaze<WanderToken>) =
    Gaze.mark gaze
    match Gaze.next gaze with
    | Ok(Integer(i)) -> Gaze.removeMark gaze; Ok(Value(WanderValue.Integer(i)))
    | _ -> Gaze.backtrack gaze; error "Could not read Integer" None

let readElement (gaze: Gaze.Gaze<WanderToken>) =
    let next = Gaze.peek gaze
    match next with
    | Error _ -> error "Error Reading Element" None
    | Ok(LetKeyword) -> readLetStatement gaze
    | Ok(Integer(_)) -> readInteger gaze
    | _ -> todo

/// Read all of the 
let readExpressions (gaze: Gaze.Gaze<WanderToken>): Result<Expression list, LigatureError> =
    let mutable res = []
    let mutable cont = true
    while cont do
        match readElement gaze with
        | Error _ -> cont <- false
        | Ok(exp) -> res <- res @ [exp]
    if Gaze.isComplete gaze then Ok(res)
    else error $"Could not match {(Gaze.peek gaze)}." None

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
        readExpressions gaze

/// Helper function that handles tokienization for you.
let parseString (input: string) =
    match tokenize input with
    | Ok(tokens) -> parse tokens
    | Error(err) -> Error(err)
