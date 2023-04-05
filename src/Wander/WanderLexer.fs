// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lexer

open Ligature

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

type WanderToken =
    | Boolean of bool
    | WhiteSpace of string
    | Identifier of Identifier
    | Integer of int64
    | Comment of string
    | NewLine of string
    | StringLiteral of string
    | BytesLiteral of byte array
    | LetKeyword
    | EqualSign
    | Name of string
    | OpenBrace
    | CloseBrace
    | Colon
    | OpenParen
    | CloseParen
    | OpenSquare
    | CloseSquare
    | Arrow
    | Dot
    | QuestionMark
    | IfKeyword
    | ElsifKeyword
    | ElseKeyword

let implode (chars: char list) =
    chars |> Array.ofList |> System.String.Concat

let takeAndMap toTake toMap =
    Gaze.map (Nibblers.takeString toTake) (fun _ -> toMap)

let identifierTokenNibbler =
    Gaze.map Lig.Read.identifierNibbler (fun chars ->
        match chars |> implode |> identifier with
        | Ok(identifier) -> Identifier(identifier)
        | Error(_) -> todo //TODO fix this when Gaze works with Results instead of Options
    )

let integerTokenNibbler =
    Gaze.map Lig.Read.integerNibbler (fun int64 -> Integer(int64))

let stringLiteralTokenNibbler =
    Gaze.map Lig.Read.stringNibbler (fun string -> StringLiteral(string))

let nameNibbler =
    Nibblers.takeAll
        [ (Nibblers.repeatN (Nibblers.takeInRange [ ('a', 'z'); ('A', 'Z'); ('_', '_') ]) 1)
          Nibblers.optional (Nibblers.repeat (Nibblers.takeInRange [ ('a', 'z'); ('A', 'Z'); ('0', '9'); ('_', '_') ])) ]

let newLineNibbler =
    Nibblers.takeFirst [ (Nibblers.takeString "\r\n"); (Nibblers.takeString "\n") ]

let newLineTokenNibbler =
    Gaze.map (Nibblers.repeat newLineNibbler) (fun text -> text |> List.concat |> implode |> NewLine)

let commentNibbler =
    Nibblers.takeAll [ Nibblers.takeString "--"; Nibblers.takeUntil newLineNibbler ] //TODO doesn't handle \r\n

let commentTokenNibbler =
    Gaze.map commentNibbler (fun commentText -> commentText |> List.concat |> implode |> Comment)

let whiteSpaceNibbler =
    Gaze.map (Nibblers.repeat (Nibblers.take ' ')) (fun ws -> ws |> implode |> WhiteSpace)

let createNameOrKeyword (name: string) =
    match name with
    | "if" -> IfKeyword
    | "elsif" -> ElsifKeyword
    | "else" -> ElseKeyword
    | "let" -> LetKeyword
    | "true" -> Boolean(true)
    | "false" -> Boolean(false)
    | _ -> Name(name)

let nameOrKeywordTokenNibbler =
    Gaze.map nameNibbler (fun chars -> chars |> List.concat |> implode |> createNameOrKeyword)

let tokenNibbler =
    Nibblers.repeat (
        Nibblers.takeFirst (
            [ whiteSpaceNibbler
              nameOrKeywordTokenNibbler
              integerTokenNibbler
              newLineTokenNibbler
              identifierTokenNibbler
              stringLiteralTokenNibbler
              takeAndMap "=" EqualSign
              takeAndMap "->" Arrow
              takeAndMap "(" OpenParen
              takeAndMap ")" CloseParen
              takeAndMap "{" OpenBrace
              takeAndMap "}" CloseBrace
              takeAndMap "." Dot
              takeAndMap "[" OpenSquare
              takeAndMap "]" CloseSquare
              takeAndMap "{" OpenBrace
              takeAndMap "}" CloseBrace
              takeAndMap ":" Colon
              takeAndMap "?" QuestionMark
              commentTokenNibbler ]
        )
    )

let tokenize script =
    let gaze = Gaze.fromString (script)
    Gaze.attempt tokenNibbler gaze
