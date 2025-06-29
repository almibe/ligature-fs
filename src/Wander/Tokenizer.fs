// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Tokenizer

open Ligature.Model
open System.Text.RegularExpressions

let identifierPattern =
    Regex("^[-a-zA-Z0-9._~:/?#\\[\\]@!$&'()*+,;%=]$¬", RegexOptions.Compiled)

let parseString (input: string) =
#if !FABLE_COMPILER
    System.Text.Json.Nodes.JsonNode.Parse(input.Replace("\n", "\\n"))
#else
    Fable.Core.JsInterop.emitJsExpr (input.Replace("\n", "\\n")) "JSON.parse($0)"
#endif

let stringContentNibbler: Gaze.Nibbler<char, string> =
    // Full pattern \"(([^\x00-\x1F\"\\]|\\[\"\\/bfnrt]|\\u[0-9a-fA-F]{4})*)\"
    Gaze.map
        (fun (chars: char list) -> parseString("\"" + System.String.Concat chars + "\"").ToString())
        (Nibblers.takeWhileAccum (fun (input, result) ->
            if input <> '"' then
                true
            else
                try
                    let s = "\"" + System.String.Concat(List.append result [ input ])
                    ignore <| parseString s
                    false
                with _ ->
                    true))

/// A Nibbler that reads Strings as defined by lig.
/// TODO: this parser is incomplete and just used for testing currently.
let stringNibbler =
    Nibblers.takeFirst
        [ Nibblers.between '"' stringContentNibbler '"'
          Gaze.map (fun _ -> "") (Nibblers.takeList [ '"'; '"' ]) ]

let charInRange char start stop = char >= start && char <= stop

let charListToInt i =
    System.Numerics.BigInteger.Parse(System.String(List.concat i |> List.toArray))

[<RequireQualifiedAccess>]
type Token =
    | WhiteSpace of string
    | NewLine of string
    | Term of string
    | Slot of string
    | Literal of string
    | Variable of string
    | OpenBrace
    | CloseBrace
    | OpenSquare
    | CloseSquare
    | OpenParen
    | CloseParen
    | Comma
    | Pipe
    | Comment

let implode (chars: char list) =
    chars |> Array.ofList |> System.String.Concat

let takeAndMap toTake toMap =
    Gaze.map (fun _ -> toMap) (Nibblers.takeString toTake)

let stringLiteralTokenNibbler =
    Gaze.map (fun string -> Token.Literal(string)) stringNibbler

// let multiLineStringLiteralTokenNibbler =
//     Gaze.map multiLineStringNibbler (fun string -> Token.StringLiteral(string))

let nameNibbler =
    Nibblers.takeAll
        [ Nibblers.repeatN
              (Nibblers.takeInRange
                  [ 'a', 'z'
                    'A', 'Z'
                    '0', '9'
                    '-', '-'
                    '+', '+'
                    '>', '>'
                    '/', '/'
                    '¬', '¬'
                    '~', '~'
                    '$', '$'
                    '?', '?'
                    '*', '*'
                    '@', '@'
                    '_', '_'
                    '=', '='
                    ':', ':' ])
              1
          Nibblers.optional (
              Nibblers.repeat (
                  Nibblers.takeInRange
                      [ 'a', 'z'
                        'A', 'Z'
                        '0', '9'
                        '?', '?'
                        '$', '$'
                        '/', '/'
                        '_', '_'
                        '>', '>'
                        '-', '-'
                        '+', '+'
                        '=', '='
                        '?', '?'
                        ':', ':'
                        '.', '.'
                        '¬', '¬' ]
              )
          ) ]

let newLineNibbler =
    Nibblers.takeFirst [ Nibblers.takeString "\r\n"; Nibblers.takeString "\n" ]

let newLineTokenNibbler =
    Gaze.map (fun text -> text |> List.concat |> implode |> Token.NewLine) (Nibblers.repeat newLineNibbler)

let commentNibbler =
    Gaze.map (fun _ -> Token.Comment) (Nibblers.takeAll [ Nibblers.takeString "--"; Nibblers.takeUntil newLineNibbler ])

let whiteSpaceNibbler =
    Gaze.map (fun ws -> ws |> implode |> Token.WhiteSpace) (Nibblers.repeat (Nibblers.take ' '))

let elementTokenNibbler =
    nameNibbler
    |> Gaze.map (fun chars ->
        chars
        |> List.concat
        |> implode
        |> fun value ->
            if value.StartsWith "?" then Token.Slot value
            else if value.StartsWith "$" then Token.Variable value
            else Token.Term value)

let tokenNibbler =
    Nibblers.optional (
        Nibblers.repeat (
            Nibblers.takeFirst
                [ commentNibbler
                  whiteSpaceNibbler
                  elementTokenNibbler
                  newLineTokenNibbler
                  stringLiteralTokenNibbler
                  takeAndMap "|" Token.Pipe
                  takeAndMap "," Token.Comma
                  takeAndMap "{" Token.OpenBrace
                  takeAndMap "}" Token.CloseBrace
                  takeAndMap "(" Token.OpenParen
                  takeAndMap ")" Token.CloseParen
                  takeAndMap "[" Token.OpenSquare
                  takeAndMap "]" Token.CloseSquare ]
        )
    )

let replaceLineEndings (script: string) =
#if !FABLE_COMPILER
    script.ReplaceLineEndings "\n"
#else
    script.Replace("\r\n", "\n")
#endif

let tokenize (script: string) =
    let gaze = Gaze.fromString (replaceLineEndings script)

    match Gaze.attempt tokenNibbler gaze with
    | Ok res ->
        if Gaze.isComplete gaze then
            Ok res
        else
            error "Error tokenizing." None
    | Error _ -> error "Error tokenizing." None
