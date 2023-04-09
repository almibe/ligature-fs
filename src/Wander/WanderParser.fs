// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module rec Ligature.Wander.Parser

open Ligature.Wander.Model
open Lexer
open FsToolkit.ErrorHandling

let todo<'T> : 'T = raise (System.NotImplementedException("todo"))

let readScopeOrLambda gaze: Result<Expression, Gaze.GazeError> =
    let scopeAttempt = 
        Gaze.attempt
            (fun gaze ->
                result {
                    let! _ = Gaze.attempt (Nibblers.take OpenBrace) gaze
                    let! expressions = readExpressionsUntil CloseBrace gaze
                    let! _ = Gaze.attempt (Nibblers.take CloseBrace) gaze
                    return Scope(expressions)
                })
            gaze
    if Result.isOk scopeAttempt then
        scopeAttempt
    else
        Gaze.attempt
            (fun gaze ->
                result {
                    let! _ = Gaze.attempt (Nibblers.take OpenBrace) gaze
                    let! names = readExpressionsUntil Arrow gaze //TODO need to make sure all read Expressions are Names
                    let names = List.map (fun expression ->
                        match expression with
                        | Expression.Name(name) -> name
                        | _ -> todo) names
                    let! _ = Gaze.attempt (Nibblers.take Arrow) gaze
                    let! expressions = readExpressionsUntil CloseBrace gaze
                    let! _ = Gaze.attempt (Nibblers.take CloseBrace) gaze
                    return Value (Lambda (names, expressions))
                })
            gaze

let nameStrNibbler (gaze: Gaze.Gaze<WanderToken>) : Result<string, Gaze.GazeError> =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Name(value)) -> Ok(value)
            | _ -> Error Gaze.GazeError.NoMatch)
        gaze

let nameNibbler =
    Nibblers.takeCond (fun token ->
        match token with
        | Name(_) -> true
        | _ -> false)

let nameExpressionNibbler gaze = Gaze.attempt (fun gaze -> todo) gaze

let readNextElement (gaze: Gaze.Gaze<WanderToken>) : Expression option = todo

let readLetStatement gaze =
    Gaze.attempt
        (fun gaze ->
            result {
                let! _ = Gaze.attempt (Nibblers.take LetKeyword) gaze
                let! name = Gaze.attempt nameStrNibbler gaze
                let! _ = Gaze.attempt (Nibblers.take EqualSign) gaze
                let! v = readExpression gaze
                return LetStatement(name, v)
            })
        gaze

let readIdentifier (gaze: Gaze.Gaze<WanderToken>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Identifier(identifier)) -> Ok(Expression.Value(WanderValue.Identifier(identifier)))
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let readArguments (gaze: Gaze.Gaze<WanderToken>) =
    result {
        let! _ = Gaze.attempt (Nibblers.take OpenParen) gaze
        let! arguments = readExpressionsUntil CloseParen gaze
        let! _ = Gaze.attempt (Nibblers.take CloseParen) gaze
        return arguments
    }

let readNameOrFunctionCall (gaze: Gaze.Gaze<WanderToken>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Name(name)) ->
                match Gaze.peek gaze with
                | Ok(OpenParen) ->
                    let arguments = readArguments gaze
                    match arguments with
                    | Ok(arguments) -> Ok(FunctionCall(name, arguments))
                    | _ -> Error(Gaze.GazeError.NoMatch)
                | _ -> Ok(Expression.Name(name))
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let readString (gaze: Gaze.Gaze<WanderToken>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(StringLiteral(value)) -> Ok(Expression.Value(WanderValue.String(value)))
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let readInteger (gaze: Gaze.Gaze<WanderToken>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Integer(i)) -> Ok(Value(WanderValue.Integer(i)))
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let readBoolean (gaze: Gaze.Gaze<WanderToken>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Boolean(b)) -> Ok(Value(WanderValue.Boolean(b)))
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let readConditional (gaze: Gaze.Gaze<WanderToken>) =
    Gaze.attempt
        (fun gaze ->
            result {
                let! _ = Gaze.attempt (Nibblers.take IfKeyword) gaze
                let! ifConditional = readExpression gaze
                let! ifBody = readExpression gaze
                
                //TODO handle elsifs
                let mutable elsifCases = []
                while Gaze.peek gaze = Ok(ElsifKeyword) do
                    let! _ = Gaze.attempt (Nibblers.take ElsifKeyword) gaze
                    let! elsifConditional = readExpression gaze
                    let! elsifBody = readExpression gaze
                    elsifCases <- elsifCases @ [{ condition = elsifConditional; body = elsifBody}]
                
                let! _ = Gaze.attempt (Nibblers.take ElseKeyword) gaze
                let! elseBody = readExpression gaze

                return
                    Conditional
                        { ifCase =
                            { condition = ifConditional
                              body = ifBody }
                          elsifCases = elsifCases
                          elseBody = elseBody }
            })
        gaze

/// Read the next Expression from the given instance of Gaze<WanderToken>
let readExpression (gaze: Gaze.Gaze<WanderToken>) : Result<Expression, Gaze.GazeError> =
    let next = Gaze.peek gaze

    match next with
    | Error(err) -> Error err
    | Ok(LetKeyword) -> readLetStatement gaze
    | Ok(OpenBrace) -> readScopeOrLambda gaze
    | Ok(Integer(_)) -> readInteger gaze
    | Ok(Boolean(_)) -> readBoolean gaze
    | Ok(Identifier(_)) -> readIdentifier gaze
    | Ok(Name(_)) -> readNameOrFunctionCall gaze //TODO will need to also handle function calls here
    | Ok(IfKeyword) -> readConditional gaze
    | Ok(StringLiteral(_)) -> readString gaze
    | _ -> Error(Gaze.GazeError.NoMatch)

/// Read all of the Expressions from a given instance of Gaze<WanderToken>
let readExpressions (gaze: Gaze.Gaze<WanderToken>) : Result<Expression list, Gaze.GazeError> =
    let mutable res = []
    let mutable cont = true

    while cont do
        match readExpression gaze with
        | Error _ -> cont <- false
        | Ok(exp) -> res <- res @ [ exp ]

    if Gaze.isComplete gaze then
        Ok(res)
    else
        Error(Gaze.GazeError.NoMatch) //error $"Could not match from {gaze.offset} - {(Gaze.remaining gaze)}." None //TODO this error message needs updated
//    printfn "%A" (sprintf "%A" (tokenize input))

/// This function is similar to readExpressions but when it reaches an Error it returns all matched expressions
/// instead of an Error.
let readExpressionsUntil (stopToken: WanderToken) (gaze: Gaze.Gaze<WanderToken>) : Result<Expression list, Gaze.GazeError> =
    let mutable res = []
    let mutable cont = true
    let mutable err = false

    while cont do
        match Gaze.peek gaze with
        | Ok(nextToken) ->
            if nextToken = stopToken then
                cont <- false
            else
                let expr = readExpression gaze
                match expr with
                | Ok(expr) -> res <- res @ [ expr ]
                | Error _ ->
                    cont <- false
                    err <- true
        | Error _ ->
            cont <- false
            err <- true
    
    if not err then Ok(res) else Error(Gaze.GazeError.NoMatch) //error $"Could not match from {gaze.offset} - {(Gaze.remaining gaze)}." None //TODO this error message needs updated

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
    | Error(err) -> Error(err) //error $"Could not match from {gaze.offset} - {(Gaze.remaining gaze)}." None //TODO this error message needs updated
//    printfn "%A" (sprintf "%A" (tokenize input))
