// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lexer
open Identifier
open LexerUtil

[<RequireQualifiedAccess>]
type Token =
    | Bool of bool
    | WhiteSpace of string
    | Identifier of Identifier
    | Int of int64
    | Comment of string
    | NewLine of string
    | StringLiteral of string
    | BytesLiteral of byte array
    | Name of string
    | OpenBrace
    | CloseBrace
    | Colon
    | OpenParen
    | CloseParen
    | OpenSquare
    | CloseSquare
    | Arrow
    | WideArrow
    | EqualsSign
    | Dot
    | QuestionMark
    | WhenKeyword
    | Hash
    | Nothing
    | Lambda
    | Comma

let implode (chars: char list) =
    chars |> Array.ofList |> System.String.Concat

let takeAndMap toTake toMap =
    Gaze.map (Nibblers.takeString toTake) (fun _ -> toMap)

let identifierTokenNibbler =
    Gaze.map identifierNibbler (fun chars ->
        match chars |> implode |> identifier with
        | Ok identifier -> Token.Identifier(identifier)
        | Error _ -> failwith "todo" //TODO fix this when Gaze works with Results instead of Options
    )

let integerTokenNibbler =
    Gaze.map integerNibbler (fun int64 -> Token.Int(int64))

let stringLiteralTokenNibbler =
    Gaze.map stringNibbler (fun string -> Token.StringLiteral(string))

let nameNibbler =
    Nibblers.takeAll
        [ (Nibblers.repeatN (Nibblers.takeInRange [ ('a', 'z'); ('A', 'Z'); ('_', '_') ]) 1)
          Nibblers.optional (Nibblers.repeat (Nibblers.takeInRange [ ('a', 'z'); ('A', 'Z'); ('0', '9'); ('_', '_'); ('.', '.') ])) ]

let newLineNibbler =
    Nibblers.takeFirst [ (Nibblers.takeString "\r\n"); (Nibblers.takeString "\n") ]

let newLineTokenNibbler =
    Gaze.map (Nibblers.repeat newLineNibbler) (fun text -> text |> List.concat |> implode |> Token.NewLine)

let commentNibbler =
    Nibblers.takeAll [ Nibblers.takeString "--"; Nibblers.takeUntil newLineNibbler ] //TODO doesn't handle \r\n

let commentTokenNibbler =
    Gaze.map commentNibbler (fun commentText -> commentText |> List.concat |> implode |> Token.Comment)

let whiteSpaceNibbler =
    Gaze.map (Nibblers.repeat (Nibblers.take ' ')) (fun ws -> ws |> implode |> Token.WhiteSpace)

let createNameOrKeyword (name: string) =
    match name with
    | "when" -> Token.WhenKeyword
    | "true" -> Token.Bool(true)
    | "false" -> Token.Bool(false)
    | "nothing" -> Token.Nothing
    | _ -> Token.Name(name)

let nameOrKeywordTokenNibbler =
    Gaze.map nameNibbler (fun chars -> chars |> List.concat |> implode |> createNameOrKeyword)

let tokenNibbler =
    Nibblers.optional (
        Nibblers.repeat (
            Nibblers.takeFirst (
                [
                whiteSpaceNibbler
                nameOrKeywordTokenNibbler
                integerTokenNibbler
                newLineTokenNibbler
                identifierTokenNibbler
                stringLiteralTokenNibbler
                takeAndMap "," Token.Comma
                takeAndMap "=>" Token.WideArrow
                takeAndMap "->" Token.Arrow
                takeAndMap "#" Token.Hash
                takeAndMap "(" Token.OpenParen
                takeAndMap ")" Token.CloseParen
                takeAndMap "{" Token.OpenBrace
                takeAndMap "}" Token.CloseBrace
                takeAndMap "." Token.Dot
                takeAndMap "[" Token.OpenSquare
                takeAndMap "]" Token.CloseSquare
                takeAndMap "{" Token.OpenBrace
                takeAndMap "}" Token.CloseBrace
                takeAndMap ":" Token.Colon
                takeAndMap "?" Token.QuestionMark
                takeAndMap "=" Token.EqualsSign
                takeAndMap "\\" Token.Lambda
                commentTokenNibbler ]
            )
        )
    )

let tokenize script =
    let gaze = Gaze.fromString (script)
    Gaze.attempt tokenNibbler gaze
