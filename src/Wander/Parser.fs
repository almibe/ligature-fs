// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module rec Ligature.Wander.Parser

open Lexer
open FsToolkit.ErrorHandling
open Ligature

[<RequireQualifiedAccess>]
type Element =
| Name of string
| String of string
| Integer of int64
| Identifier of Identifier
| List of Element list


// let readScopeOrLambda gaze: Result<Expression, Gaze.GazeError> =
//     let scopeAttempt = 
//         Gaze.attempt
//             (fun gaze ->
//                 result {
//                     let! _ = Gaze.attempt (Nibblers.take Token.OpenBrace) gaze
//                     let! expressions = readExpressionsUntil Token.CloseBrace gaze
//                     let! _ = Gaze.attempt (Nibblers.take Token.CloseBrace) gaze
//                     return Expression.Scope(expressions)
//                 })
//             gaze
//     if Result.isOk scopeAttempt then
//         scopeAttempt
//     else
//         Gaze.attempt
//             (fun gaze ->
//                 result {
//                     let! _ = Gaze.attempt (Nibblers.take Token.OpenBrace) gaze
//                     let! names = readExpressionsUntil Token.Arrow gaze //TODO need to make sure all read Expressions are Names
//                     let names = List.map (fun expression ->
//                         match expression with
//                         | Expression.Name(name) -> name
//                         | _ -> failwith "todo") names
//                     let! _ = Gaze.attempt (Nibblers.take Token.Arrow) gaze
//                     let! expressions = readExpressionsUntil Token.CloseBrace gaze
//                     let! _ = Gaze.attempt (Nibblers.take Token.CloseBrace) gaze
//                     return Expression.Value (WanderValue.Lambda (names, expressions))
//                 })
//             gaze

// let nameStrNibbler (gaze: Gaze.Gaze<Token>) : Result<string, Gaze.GazeError> =
//     Gaze.attempt
//         (fun gaze ->
//             match Gaze.next gaze with
//             | Ok(Token.Name(value)) -> Ok(value)
//             | _ -> Error Gaze.GazeError.NoMatch)
//         gaze

// let nameNibbler =
//     Nibblers.takeCond (fun token ->
//         match token with
//         | Token.Name _ -> true
//         | _ -> false)

// let nameExpressionNibbler gaze = Gaze.attempt (fun gaze -> failwith "todo") gaze

// let readNextElement (gaze: Gaze.Gaze<Token>) : Expression option = failwith "todo"

// let readLetStatement gaze =
//     Gaze.attempt
//         (fun gaze ->
//             result {
//                 let! _ = Gaze.attempt (Nibblers.take Token.LetKeyword) gaze
//                 let! name = Gaze.attempt nameStrNibbler gaze
//                 let! _ = Gaze.attempt (Nibblers.take Token.EqualSign) gaze
//                 let! v = readExpression gaze
//                 return Expression.LetStatement(name, v)
//             })
//         gaze

// let readIdentifier (gaze: Gaze.Gaze<Token>) =
//     Gaze.attempt
//         (fun gaze ->
//             match Gaze.next gaze with
//             | Ok(Token.Identifier(identifier)) -> Ok(Expression.Value(WanderValue.Identifier(identifier)))
//             | _ -> Error(Gaze.GazeError.NoMatch))
//         gaze

// let readArguments (gaze: Gaze.Gaze<Token>) =
//     result {
//         let! _ = Gaze.attempt (Nibblers.take Token.OpenParen) gaze
//         let! arguments = readExpressionsUntil Token.CloseParen gaze
//         let! _ = Gaze.attempt (Nibblers.take Token.CloseParen) gaze
//         return arguments
//     }

// let readNameOrFunctionCall (gaze: Gaze.Gaze<Token>) =
//     Gaze.attempt
//         (fun gaze ->
//             match Gaze.next gaze with
//             | Ok(Token.Name(name)) ->
//                 match Gaze.peek gaze with
//                 | Ok(Token.OpenParen) ->
//                     let arguments = readArguments gaze
//                     match arguments with
//                     | Ok(arguments) -> Ok(Expression.FunctionCall(name, arguments))
//                     | _ -> Error(Gaze.GazeError.NoMatch)
//                 | _ -> Ok(Expression.Name(name))
//             | _ -> Error(Gaze.GazeError.NoMatch))
//         gaze

// let readString (gaze: Gaze.Gaze<Token>) =
//     Gaze.attempt
//         (fun gaze ->
//             match Gaze.next gaze with
//             | Ok(Token.StringLiteral(value)) -> Ok(Expression.Value(WanderValue.String(value)))
//             | _ -> Error(Gaze.GazeError.NoMatch))
//         gaze

let readInteger (gaze: Gaze.Gaze<Token>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.Integer(i)) -> Ok(Element.Integer(i))
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

// let readBoolean (gaze: Gaze.Gaze<Token>) =
//     Gaze.attempt
//         (fun gaze ->
//             match Gaze.next gaze with
//             | Ok(Token.Boolean(b)) -> Ok(Expression.Value(WanderValue.Boolean(b)))
//             | _ -> Error(Gaze.GazeError.NoMatch))
//         gaze

// let readConditional (gaze: Gaze.Gaze<Token>) =
//     Gaze.attempt
//         (fun gaze ->
//             result {
//                 let! _ = Gaze.attempt (Nibblers.take Token.IfKeyword) gaze
//                 let! ifConditional = readExpression gaze
//                 let! ifBody = readExpression gaze
                
//                 let mutable elsifCases = []
//                 while Gaze.peek gaze = Ok(Token.ElsifKeyword) do
//                     let! _ = Gaze.attempt (Nibblers.take Token.ElsifKeyword) gaze
//                     let! elsifConditional = readExpression gaze
//                     let! elsifBody = readExpression gaze
//                     elsifCases <- elsifCases @ [{ condition = elsifConditional; body = elsifBody}]
                
//                 let! _ = Gaze.attempt (Nibblers.take Token.ElseKeyword) gaze
//                 let! elseBody = readExpression gaze

//                 return
//                     Expression.Conditional
//                         { ifCase =
//                             { condition = ifConditional
//                               body = ifBody }
//                           elsifCases = elsifCases
//                           elseBody = elseBody }
//             })
//         gaze

// let readTuple (gaze: Gaze.Gaze<Token>) : Result<Expression, Gaze.GazeError> =
//     result {
//         let! _ = Gaze.attempt (Nibblers.take Token.OpenParen) gaze
//         let! arguments = readExpressionsUntil Token.CloseParen gaze
//         let! _ = Gaze.attempt (Nibblers.take Token.CloseParen) gaze
//         return Expression.TupleExpression(arguments)
//     }

/// Read the next Element from the given instance of Gaze<Token>
let readElement (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    let next = Gaze.peek gaze

    match next with
    | Error(err) -> Error err
    // | Ok(Token.LetKeyword) -> readLetStatement gaze
    // | Ok(Token.OpenBrace) -> readScopeOrLambda gaze
    // | Ok(Token.OpenParen) -> readTuple gaze
    | Ok(Token.Integer(_)) -> readInteger gaze
    // | Ok(Token.Boolean(_)) -> readBoolean gaze
    // | Ok(Token.Identifier(_)) -> readIdentifier gaze
    // | Ok(Token.Name(_)) -> readNameOrFunctionCall gaze //TODO will need to also handle function calls here
    // | Ok(Token.IfKeyword) -> readConditional gaze
    // | Ok(Token.StringLiteral(_)) -> readString gaze
    | _ -> Error(Gaze.GazeError.NoMatch)

/// Read all of the Elements from a given instance of Gaze<Token>
let readElements (gaze: Gaze.Gaze<Token>) : Result<Element list, Gaze.GazeError> =
    let mutable res = []
    let mutable cont = true

    while cont do
        match readElement gaze with
        | Error _ -> cont <- false
        | Ok(exp) -> res <- res @ [ exp ]

    if Gaze.isComplete gaze then
        Ok(res)
    else
        Error(Gaze.GazeError.NoMatch) //error $"Could not match from {gaze.offset} - {(Gaze.remaining gaze)}." None //TODO this error message needs updated
//    printfn "%A" (sprintf "%A" (tokenize input))

// /// This function is similar to readExpressions but when it reaches an Error it returns all matched expressions
// /// instead of an Error.
// let readExpressionsUntil (stopToken: Token) (gaze: Gaze.Gaze<Token>) : Result<Expression list, Gaze.GazeError> =
//     let mutable res = []
//     let mutable cont = true
//     let mutable err = false

//     while cont do
//         match Gaze.peek gaze with
//         | Ok(nextToken) ->
//             if nextToken = stopToken then
//                 cont <- false
//             else
//                 let expr = readExpression gaze
//                 match expr with
//                 | Ok(expr) -> res <- res @ [ expr ]
//                 | Error _ ->
//                     cont <- false
//                     err <- true
//         | Error _ ->
//             cont <- false
//             err <- true
    
//     if not err then Ok(res) else Error(Gaze.GazeError.NoMatch) //error $"Could not match from {gaze.offset} - {(Gaze.remaining gaze)}." None //TODO this error message needs updated

/// <summary></summary>
/// <param name="tokens">The list of Tokens to be parsered.</param>
/// <returns>The AST created from the token list of an Error.</returns>
let parse (tokens: Token list): Result<Element list, Gaze.GazeError> =
    let tokens =
        List.filter
            (fun token ->
                match token with
                | Token.Comment(_)
                | Token.WhiteSpace(_)
                | Token.NewLine(_) -> false
                | _ -> true)
            tokens

    if tokens.IsEmpty then
        Ok []
    else
        let gaze = Gaze.fromList tokens
        readElements gaze

/// Helper function that handles tokienization for you.
let parseString (input: string) =
    match tokenize input with
    | Ok tokens -> parse tokens
    | Error err -> Error err //error "Could not parse input." None //error $"Could not match from {gaze.offset} - {(Gaze.remaining gaze)}." None //TODO this error message needs updated
//    printfn "%A" (sprintf "%A" (tokenize input))
