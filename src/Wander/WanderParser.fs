// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module rec Ligature.Wander.Parser

open Ligature.Wander.Model
open Ligature
open Lexer
open FsToolkit.ErrorHandling

let todo<'T> : 'T = raise (System.NotImplementedException("todo"))

//let simpleValueNibbler = (Nibblers.takeFirst [ integerNibbler; stringNibbler; booleanNibbler; identifierNibbler ])

// let valueNibbler gaze =
//     Gaze.attempt (fun gaze ->
//         match Gaze.next gaze with
//         | Ok(Integer(i)) -> Ok(WanderValue.Integer(i))
//         | Ok(StringLiteral(s)) -> Ok(WanderValue.String(s))
//         | Ok(Boolean(b)) -> Ok(WanderValue.Boolean(b))
//         | Ok(Identifier(i)) -> Ok(WanderValue.Identifier(i))
//         | _ -> todo) //error $"Token {e} can not start an expression or statement." None)
//         gaze

// let valueExpressionNibbler =
//     Gaze.map simpleValueNibbler (fun token ->
//         match token with
//         | Integer(i) -> Value(WanderValue.Integer(i))
//         | StringLiteral(s) -> Value(WanderValue.String(s))
//         | Boolean(b) -> Value(WanderValue.Boolean(b))
//         | Identifier(i) -> Value(WanderValue.Identifier(i))
//         | _ -> Value(WanderValue.Nothing)) //error $"Token {e} can not start an expression or statement." None)

let readScope gaze =
    todo
    //Nibblers.between OpenBrace expressionNibbler CloseBrace

let scopeExpressionNibbler =
    todo
//    Gaze.map (scopeNibbler ()) (fun expr ->
//        Scope [])

let nameStrNibbler (gaze: Gaze.Gaze<WanderToken>): Result<string, Gaze.GazeError> =
    Gaze.attempt (fun gaze ->
        match Gaze.next gaze with
        | Ok(Name(value)) -> Ok(value)
        | _ -> Error Gaze.GazeError.NoMatch)
        gaze

let nameNibbler =
    Nibblers.takeCond (fun token ->
        match token with
        | Name(_) -> true
        | _ -> false)

let nameExpressionNibbler gaze =
    Gaze.attempt (fun gaze ->
        todo)
        gaze

// let letStatementNibbler =
//     Gaze.map
//         (Nibblers.takeAll
//             [ Nibblers.take LetKeyword
//               nameNibbler
//               Nibblers.take EqualSign
//               simpleValueNibbler ])
//         (fun tokens ->
//             match tokens with
//             | [ _; Name(name); _; Integer(i) ] -> LetStatement(name, Value(WanderValue.Integer(i)))
//             | [ _; Name(name); _; Boolean(b) ] -> LetStatement(name, Value(WanderValue.Boolean(b)))
//             | [ _; Name(name); _; StringLiteral(s)  ] -> LetStatement(name, Value(WanderValue.String(s)))
//             | [ _; Name(name); _; Identifier(i)] -> LetStatement(name, Value(WanderValue.Identifier(i)))
//             | _ -> todo)

// let expressionNibbler =
//     Nibblers.repeat (Nibblers.takeFirst [
//         scopeExpressionNibbler
//         valueExpressionNibbler
//         letStatementNibbler
//         nameExpressionNibbler
//         ])

let readNextElement (gaze: Gaze.Gaze<WanderToken>): Expression option =
    todo

let readLetStatement gaze =
    Gaze.attempt (fun gaze ->
        result {
            let! _ = Gaze.attempt (Nibblers.take LetKeyword) gaze
            let! name = Gaze.attempt nameStrNibbler gaze
            let! _ = Gaze.attempt (Nibblers.take EqualSign) gaze
            let! v = readExpression gaze
            return LetStatement (name, v)
        })
        gaze

let readIdentifier (gaze: Gaze.Gaze<WanderToken>) =
    Gaze.attempt (fun gaze ->
        match Gaze.next gaze with
        | Ok(Identifier(identifier)) -> Ok(Expression.Value(WanderValue.Identifier(identifier)))
        | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let readName (gaze: Gaze.Gaze<WanderToken>) =
    Gaze.attempt (fun gaze ->
        match Gaze.next gaze with
        | Ok(Name(name)) -> Ok(Expression.Name(name))
        | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let readString (gaze: Gaze.Gaze<WanderToken>) =
    Gaze.attempt (fun gaze ->
        match Gaze.next gaze with
        | Ok(StringLiteral(value)) -> Ok(Expression.Value(WanderValue.String(value)))
        | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let readInteger (gaze: Gaze.Gaze<WanderToken>) =
    Gaze.attempt (fun gaze ->
        match Gaze.next gaze with
        | Ok(Integer(i)) -> Ok(Value(WanderValue.Integer(i)))
        | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let readBoolean (gaze: Gaze.Gaze<WanderToken>) =
    Gaze.attempt (fun gaze ->
        match Gaze.next gaze with
        | Ok(Boolean(b)) -> Ok(Value(WanderValue.Boolean(b)))
        | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

/// Read the next Expression from the given instance of Gaze<WanderToken>
let readExpression (gaze: Gaze.Gaze<WanderToken>): Result<Expression, Gaze.GazeError> =
    let next = Gaze.peek gaze
    match next with
    | Error(err) -> Error err
    | Ok(LetKeyword) -> readLetStatement gaze
    | Ok(Integer(_)) -> readInteger gaze
    | Ok(Boolean(_)) -> readBoolean gaze
    | Ok(Identifier(_)) -> readIdentifier gaze
    | Ok(Name(_)) -> readName gaze
    | Ok(StringLiteral(_)) -> readString gaze
    | _ -> Error(Gaze.GazeError.NoMatch)

/// Read all of the Expressions from a given instance of Gaze<WanderToken>
let readExpressions (gaze: Gaze.Gaze<WanderToken>): Result<Expression list, LigatureError> =
    let mutable res = []
    let mutable cont = true
    while cont do
        match readExpression gaze with
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
