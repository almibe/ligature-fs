// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module rec Ligature.Wander.Parser

open Lexer
open FsToolkit.ErrorHandling
open Model
open Nibblers

[<RequireQualifiedAccess>]
type Element =
| Name of string
| Nothing
| Grouping of Element list
| Application of Element list
| String of string
| Int of int64
| Bool of bool
| Identifier of Identifier.Identifier
| Array of Element list
| Let of string * Element
| When of (Element * Element) list
| Lambda of string list * Element
| Record of (string * Element) list

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

let nameStrNibbler (gaze: Gaze.Gaze<Token>) : Result<string, Gaze.GazeError> =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.Name(value)) -> Ok(value)
            | _ -> Error Gaze.GazeError.NoMatch)
        gaze

// let nameNibbler =
//     Nibblers.takeCond (fun token ->
//         match token with
//         | Token.Name _ -> true
//         | _ -> false)

// let nameExpressionNibbler gaze = Gaze.attempt (fun gaze -> failwith "todo") gaze

// let readNextElement (gaze: Gaze.Gaze<Token>) : Expression option = failwith "todo"

let readLetStatement gaze =
    Gaze.attempt
        (fun gaze ->
            result {
                let! _ = Gaze.attempt (take Token.LetKeyword) gaze
                let! name = Gaze.attempt nameStrNibbler gaze
                let! v = elementNib gaze
                return Element.Let(name, v)
            })
        gaze

let lambdaNib gaze =
    Gaze.attempt
        (fun gaze ->
            result {
                let! _ = Gaze.attempt (take Token.Lambda) gaze
                let! parameters = Gaze.attempt (repeat nameStrNibbler) gaze
                let! _ = Gaze.attempt (take Token.Arrow) gaze
                let! body = Gaze.attempt elementNib gaze
                return Element.Lambda(parameters, body)
            }
        )
        gaze

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
            | Ok(Token.Int(i)) -> Ok(Element.Int(i))
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let nameNib (gaze: Gaze.Gaze<Token>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.Name(name)) -> Ok(Element.Name(name))
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let equalSignNib (gaze: Gaze.Gaze<Token>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.EqualsSign) -> Ok(())
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

let arrayNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenSquare) gaze
        let! values = Gaze.attempt (repeat elementNib) gaze
        let! _ = Gaze.attempt (take Token.CloseSquare) gaze
        return Element.Array(values)
    }

let declarationsNib (gaze: Gaze.Gaze<Token>) =
    result {
        let! name = Gaze.attempt nameStrNibbler gaze
        let! _ = Gaze.attempt equalSignNib gaze
        let! expression = Gaze.attempt elementNib gaze
        return (name, expression)
    }

let recordNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenBrace) gaze
        let! declarations = (repeatOptional declarationsNib) gaze
        let! _ = Gaze.attempt (take Token.CloseBrace) gaze
        return Element.Record(declarations)
    }

/// Read the next Element from the given instance of Gaze<Token>
let readValue (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Int(value)) -> Ok(Element.Int value)
    | Ok(Token.Bool(value)) -> Ok(Element.Bool value)
    | Ok(Token.Identifier(value)) -> Ok(Element.Identifier value)
    | Ok(Token.Name(name)) -> Ok(Element.Name(name)) //readNameOrFunctionCall gaze //TODO will need to also handle function calls here
    | Ok(Token.StringLiteral(value)) -> Ok(Element.String value)
    | _ -> Error(Gaze.GazeError.NoMatch)

let elementNib = takeFirst [
    readValue; 
    readLetStatement; 
    arrayNib; 
    recordNib;
    lambdaNib
    ]

/// <summary></summary>
/// <param name="tokens">The list of Tokens to be parsered.</param>
/// <returns>The AST created from the token list of an Error.</returns>
let parse (tokens: Token list): Result<Element, Gaze.GazeError> =
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
        Ok Element.Nothing
    else
        let gaze = Gaze.fromList tokens
        Gaze.attempt elementNib gaze

/// Helper function that handles tokienization for you.
let parseString (input: string) =
    match tokenize input with
    | Ok tokens -> parse tokens
    | Error err -> Error err //error "Could not parse input." None //error $"Could not match from {gaze.offset} - {(Gaze.remaining gaze)}." None //TODO this error message needs updated
//    printfn "%A" (sprintf "%A" (tokenize input))

let expressArray values =
    let res = List.map (fun value -> express value) values
    Expression.Array res

let handleRecord (declarations: list<string * Element>) =
    let res = List.map (fun (name, value) -> (name, (express value))) declarations
    Expression.Record res

let handleLambda (parameters: string list) body =
    Expression.Lambda (parameters, (express body))

/// This will eventually handle processing pipe operators
let express (element: Element) =
    match element with
    | Element.Int value -> Expression.Int value
    | Element.Bool value -> Expression.Bool value
    | Element.Name name -> Expression.Name name
    | Element.Nothing -> Expression.Nothing
    | Element.String value -> Expression.String value
    | Element.Identifier id -> Expression.Identifier id
    | Element.Let(name,value) -> Expression.Let(name, (express value))
    | Element.Array values -> expressArray values
    | Element.Grouping elements -> failwith "todo"
    | Element.Application elements -> failwith "todo"
    | Element.Record declarations -> handleRecord declarations
    | Element.Lambda(parameters, body) -> handleLambda parameters body
    | Element.When(_) -> failwith "todo"
