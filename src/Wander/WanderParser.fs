// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Parser

open Ligature
open Ligature.Wander.Model

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

// type WanderToken =
// | Boolean of bool
// | WhiteSpace of string
// | Identifier of Identifier
// | Integer of int64
// | Comment of string 
// | NewLine of string
// | StringLiteral of string
// | BytesLiteral of byte array
// | LetKeyword
// | EqualSign
// | Name of string
// | OpenBrace
// | CloseBrace
// | Colon
// | OpenParen
// | CloseParen
// | OpenSquare
// | CloseSquare
// | Arrow
// | Dot
// | QuestionMark
// | IfKeyword
// | ElsifKeyword
// | ElseKeyword

let implode (chars: char list) =
    chars
    |> Array.ofList
    |> System.String.Concat

let takeAndMap toTake toMap =
    Gaze.map (Nibblers.takeString toTake) (fun _ -> toMap)

let identifierTokenNibbler = Gaze.map Lig.Read.identifierNibbler (fun chars -> 
    match chars |> implode |> identifier with
    | Ok(identifier) -> Identifier(identifier)
    | Error(_) -> todo //TODO fix this when Gaze works with Results instead of Options
    )

//let integerTokenNibbler = Gaze.map Lig.Read.integerNibbler (fun int64 -> Integer(int64))

//let stringLiteralTokenNibbler = Gaze.map Lig.Read.stringNibbler (fun string -> StringLiteral(string))

let nameNibbler = Gaze.map (Nibblers.takeAll [
    (Nibblers.repeatN (Nibblers.takeInRange [('a', 'z'); ('A', 'Z'); ('_', '_')]) 1)
    (Nibblers.repeat (Nibblers.takeInRange [('a', 'z'); ('A', 'Z'); ('0', '9'); ('_', '_')]))]) (fun chars ->
    chars
    |> List.concat
    |> implode
    |> Name)

let newLineNibbler = Nibblers.takeFirst [(Nibblers.takeString "\r\n"); (Nibblers.takeString "\n")]

//let newLineTokenNibbler = Gaze.map (Nibblers.repeat newLineNibbler) (fun text -> text |> List.concat |> implode |> NewLine)

let commentNibbler = Nibblers.takeAll [
    Nibblers.takeString "--"
    Nibblers.takeUntil newLineNibbler //TODO doesn't handle \r\n
    ]

// let commentTokenNibbler = Gaze.map commentNibbler (fun commentText -> commentText |> List.concat |> implode |> Comment)

// let whiteSpaceNibbler = Gaze.map (Nibblers.repeat (Nibblers.take ' ')) (fun ws -> ws |> implode |> WhiteSpace)

// let createNameOrKeyword (name: string) =
//     match name with
//     | "if" -> IfKeyword
//     | "elsif" -> ElsifKeyword
//     | "else" -> ElseKeyword
//     | "let" -> LetKeyword
//     | "true" -> Boolean(true)
//     | "false" -> Boolean(false)
//     | _ -> Name(name)

// let nameOrKeywordTokenNibbler = Gaze.map nameNibbler (fun chars -> 
//     chars
//     |> List.concat
//     |> implode
//     |> createNameOrKeyword
// )

// let tokenNibbler = Nibblers.repeat(Nibblers.takeFirst([
//     whiteSpaceNibbler
//     nameOrKeywordTokenNibbler
//     integerTokenNibbler
//     newLineTokenNibbler
//     identifierTokenNibbler
//     stringLiteralTokenNibbler
//     takeAndMap "=" EqualSign
//     takeAndMap "->" Arrow
//     takeAndMap "(" OpenParen
//     takeAndMap ")" CloseParen
//     takeAndMap "{" OpenBrace
//     takeAndMap "}" CloseBrace
//     takeAndMap "." Dot
//     takeAndMap "[" OpenSquare
//     takeAndMap "]" CloseSquare
//     takeAndMap "{" OpenBrace
//     takeAndMap "}" CloseBrace
//     takeAndMap ":" Colon
//     takeAndMap "?" QuestionMark
//     commentTokenNibbler
// ]))

// let tokenize script =
//     let gaze = Gaze.fromString(script)
//     let tokens = Gaze.attempt tokenNibbler gaze
//     match tokens with
//     | Some(tokens) -> Ok(tokens)
//     | None -> error "Could not read token" None

let integerNibbler = Gaze.map Lig.Read.integerNibbler (fun value -> 
    value 
    |> Integer 
    |> Value
    )

let parserNibbler = Nibblers.repeat (Nibblers.takeFirst [
    integerNibbler
    nameNibbler
    ])

let parse input: Result<list<Expression>, LigatureError> = 
    let gaze = Gaze.fromString input
    let tokens = Gaze.attempt parserNibbler gaze
    match tokens with
    | Some(tokens) -> Ok(tokens)
    | None -> error "Could not read token" None

//module Ligature.Wander.Parser

// open Ligature.Wander.Model
// open Ligature
// open Lexer

// let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

// let nextName tokens index = todo

// // let nextLetStatement tokens index =
// //     if (not tokens[index] = LetKeyword) || (not tokens[index+2] = EqualSign) then
// //         error "Could not read let statement." None
// //     else
//     //assert that tokens[index] is LetKeyword
//     //read name from tokens[index + 1]
//     //assert that tokens[index + 2] is Equal sign
//     //call next token on [index + 3]


// // let nextToken (tokens: WanderToken array) index =
// //     match tokens[index] with
// //     | Integer(value) -> Ok((Value(WanderValue.Integer(value)), index + 1))
// //     | StringLiteral(value) -> Ok((Value(WanderValue.String(value))), index + 1)
// //     | Boolean(value) -> Ok((Value(WanderValue.Boolean(value))), index + 1)
// //     | Identifier(value) -> Ok((Value(WanderValue.Identifier(value))), index + 1)
// //     | LetKeyword -> nextLetStatement tokens index
// //     | e -> error $"Token {e} can not start an expression or statement." None

// /// <summary></summary>
// /// <param name="tokens">The list of WanderTokens to be parsered.</param>
// /// <returns>The AST created from the token list of an Error.</returns>
// // let parse (tokens: Lexer.WanderToken list) =
// //     let tokens = tokens |> List.filter (fun token -> match token with | NewLine(_) | WhiteSpace(_) -> false | _ -> true) |> List.toArray
// //     let mutable index = 0
// //     let mutable ast = []
// //     let mutable error = None
// //     while index < (Array.length tokens) && error.IsNone do
// //         let nextToken = nextToken tokens index
// //         match nextToken with
// //         | Error(err) -> error <- Some(err)
// //         | Ok((token, newIndex)) -> 
// //             ast <- List.append ast [token]
// //             index <- newIndex
// //     match error with
// //     | None -> Ok(ast)
// //     | Some(err) -> Error(err)
