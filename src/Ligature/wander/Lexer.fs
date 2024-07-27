// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lexer

open Ligature.Main
open LexerUtil
open Fable.Core.JsInterop

[<RequireQualifiedAccess>]
type Token =
    | WhiteSpace of string
    // | Identifier of Identifier
    | Slot of Slot
    | Int of bigint
    | Bytes of byte array
    | Comment of string
    | NewLine of string
    | StringLiteral of string
    | BytesLiteral of byte array
    | Word of string
    | OpenBrace
    | CloseBrace
    | Colon
    | OpenParen
    | CloseParen
    | OpenSquare
    | CloseSquare
    | Arrow
    | WideArrow
    | Asterisk
    | Hash
    | Comma

let implode (chars: char list) =
    chars |> Array.ofList |> System.String.Concat

let takeAndMap toTake toMap =
    Gaze.map (Nibblers.takeString toTake) (fun _ -> toMap)

// let identifierTokenNibbler =
//     Gaze.map identifierNibbler (fun chars ->
//         match chars |> implode |> identifier with
//         | Ok identifier -> Token.Identifier(identifier)
//         | Error _ -> failwith "todo" //TODO fix this when Gaze works with Results instead of Options
//     )

let slotTokenNibbler =
    Gaze.map slotNibbler (fun chars ->
        if chars = [ '$' ] then
            Token.Slot(Slot(None))
        else
            chars.[1..] |> implode |> Some |> Slot |> Token.Slot)

let bytesFromString (s: string) =
#if !FABLE_COMPILER
    System.Convert.FromHexString(s)
#else
    emitJsExpr s "Uint8Array.from($0.match(/.{1,2}/g).map((byte) => parseInt(byte, 16)));"
#endif

let bytesTokenNibbler =
    Gaze.map bytesNibbler (fun value ->
        let bytes = System.String.Concat(Array.ofList (List.concat value))
        Token.Bytes(bytesFromString (bytes.[2..])))

let integerTokenNibbler = Gaze.map integerNibbler (fun int -> Token.Int(int))

let stringLiteralTokenNibbler =
    Gaze.map stringNibbler (fun string -> Token.StringLiteral(string))

let nameNibbler =
    Nibblers.takeAll
        [ (Nibblers.repeatN (Nibblers.takeInRange [ ('a', 'z'); ('A', 'Z'); ('?', '?'); ('_', '_'); ('=', '=') ]) 1)
          Nibblers.optional (
              Nibblers.repeat (
                  Nibblers.takeInRange
                      [ ('a', 'z')
                        ('A', 'Z')
                        ('0', '9')
                        ('?', '?')
                        ('_', '_')
                        ('=', '=')
                        ('.', '.') ]
              )
          ) ]

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

let nameOrKeywordTokenNibbler =
    Gaze.map nameNibbler (fun chars -> chars |> List.concat |> implode |> Token.Word)

let tokenNibbler =
    Nibblers.optional (
        Nibblers.repeat (
            Nibblers.takeFirst (
                [ whiteSpaceNibbler
                  nameOrKeywordTokenNibbler
                  bytesTokenNibbler
                  integerTokenNibbler
                  newLineTokenNibbler
                  //                  identifierTokenNibbler
                  slotTokenNibbler
                  stringLiteralTokenNibbler
                  takeAndMap "," Token.Comma
                  takeAndMap "=>" Token.WideArrow
                  takeAndMap "->" Token.Arrow
                  takeAndMap "*" Token.Asterisk
                  takeAndMap "#" Token.Hash
                  takeAndMap "(" Token.OpenParen
                  takeAndMap ")" Token.CloseParen
                  takeAndMap "{" Token.OpenBrace
                  takeAndMap "}" Token.CloseBrace
                  takeAndMap "[" Token.OpenSquare
                  takeAndMap "]" Token.CloseSquare
                  takeAndMap "{" Token.OpenBrace
                  takeAndMap "}" Token.CloseBrace
                  takeAndMap ":" Token.Colon
                  commentTokenNibbler ]
            )
        )
    )

let tokenize script =
    let gaze = Gaze.fromString (script)

    match Gaze.attempt tokenNibbler gaze with
    | Ok res ->
        if Gaze.isComplete gaze then
            Ok res
        else
            error "Error tokenizing." None
    | Error _ -> error "Error tokenizing." None
