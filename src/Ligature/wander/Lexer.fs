// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lexer

open Ligature.Main

open System.Text.RegularExpressions

let identifierPattern =
    Regex("^[-a-zA-Z0-9._~:/?#\\[\\]@!$&'()*+,;%=]$", RegexOptions.Compiled)

let identifierCharacterNibbler =
    Nibblers.takeWhile (fun c -> identifierPattern.IsMatch(c.ToString()))

let identifierNibbler = Nibblers.between '`' identifierCharacterNibbler '`'

let networkNamePattern = Regex("^[a-zA-Z0-9_.]$", RegexOptions.Compiled)

let slotPattern = Regex("^[a-zA-Z0-9_]$", RegexOptions.Compiled)

let slotCharacterNibbler =
    Nibblers.takeWhile (fun c -> slotPattern.IsMatch(c.ToString()))

let slotNibbler =
    Nibblers.takeAllFlatten
        [ Nibblers.takeAll [ Nibblers.take '$' ]
          Nibblers.optional slotCharacterNibbler ]

let networkNameCharacterNibbler =
    Nibblers.takeWhile (fun c -> networkNamePattern.IsMatch(c.ToString()))

let networkNameNibbler =
    Nibblers.takeAllFlatten
        [ Nibblers.takeAll [ Nibblers.take '@' ]
          Nibblers.optional networkNameCharacterNibbler ]

let parseString (input: string) =
#if !FABLE_COMPILER
    System.Text.Json.Nodes.JsonNode.Parse(input)
#else
    Fable.Core.JsInterop.emitJsExpr (input) "JSON.parse($0)"
#endif

let stringContentNibbler: Gaze.Nibbler<char, string> =
    // Full pattern \"(([^\x00-\x1F\"\\]|\\[\"\\/bfnrt]|\\u[0-9a-fA-F]{4})*)\"
    Gaze.map
        (Nibblers.takeWhileAccum (fun (input, result) ->
            if input <> '"' then
                true
            else
                (try
                    (let s = "\"" + System.String.Concat((List.append result [ input ]))
                     ignore <| parseString s
                     false)
                 with _ ->
                     true)))
        (fun chars -> parseString("\"" + System.String.Concat(chars) + "\"").ToString())

/// A Nibbler that reads Strings as defined by lig.
/// TODO: this parser is incomplete and just used for testing currently.
let stringNibbler =
    Nibblers.takeFirst (
        [ Nibblers.between '"' stringContentNibbler '"'
          Gaze.map (Nibblers.takeList [ '"'; '"' ]) (fun _ -> "") ]
    )

//let stringTokenNibbler = Gaze.map stringNibbler (fun s -> Token.StringLiteral(s))

let charInRange char start stop = char >= start && char <= stop

let charListToInt i =
    System.Numerics.BigInteger.Parse(System.String(List.concat i |> List.toArray))

/// A Nibbler that consumes an integer as defined by lig.
/// TODO: this doesn't handle all cases well like too small or large of a number
let integerNibbler =
    Gaze.map
        (Nibblers.takeAll
            [ (Nibblers.optional (Nibblers.takeString "-"))
              Nibblers.takeWhile (fun c -> charInRange c '0' '9') ])
        (fun i -> charListToInt i)

let bytesNibbler =
    Nibblers.takeAll
        [ Nibblers.takeString "0x"
          Nibblers.takeWhile (fun c -> charInRange c '0' '9' || charInRange c 'A' 'F') ]

let whitespaceNibbler = Nibblers.takeWhile (fun c -> c = ' ' || c = '\t')

[<RequireQualifiedAccess>]
type Token =
    | WhiteSpace of string
    | NewLine of string
    | Slot of Slot
    | Name of string
    | NetworkName of string
    | StringLiteral of string
    | Int of bigint
    | OpenBrace
    | CloseBrace
    | OpenSquare
    | CloseSquare
    | Comma

let implode (chars: char list) =
    chars |> Array.ofList |> System.String.Concat

let takeAndMap toTake toMap =
    Gaze.map (Nibblers.takeString toTake) (fun _ -> toMap)

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
    Fable.Core.JsInterop.emitJsExpr s "Uint8Array.from($0.match(/.{1,2}/g).map((byte) => parseInt(byte, 16)));"
#endif

let integerTokenNibbler = Gaze.map integerNibbler (fun int -> Token.Int(int))

let networkNameTokenNibbler =
    Gaze.map networkNameNibbler (fun chars ->
        if chars = [ '@' ] then
            Token.NetworkName("")
        else
            let name = chars.[1..] |> implode
            // if name.Contains(".") then
            //     match name.Split(".") with
            //     | [| networkName; name |] -> 
            //         Token.QualifiedName (networkName, name)
            //     | _ -> failwith "TODO"
            // else
            Token.NetworkName name)

let stringLiteralTokenNibbler =
    Gaze.map stringNibbler (fun string -> Token.StringLiteral(string))

let nameNibbler =
    Nibblers.takeAll
        [ (Nibblers.repeatN
              (Nibblers.takeInRange [ ('a', 'z'); ('A', 'Z'); ('?', '?'); ('_', '_'); ('=', '='); (':', ':') ])
              1)
          Nibblers.optional (
              Nibblers.repeat (
                  Nibblers.takeInRange
                      [ ('a', 'z')
                        ('A', 'Z')
                        ('0', '9')
                        ('?', '?')
                        ('_', '_')
                        ('-', '-')
                        ('=', '=')
                        (':', ':')
                        ('.', '.') ]
              )
          ) ]

let newLineNibbler =
    Nibblers.takeFirst [ (Nibblers.takeString "\r\n"); (Nibblers.takeString "\n") ]

let newLineTokenNibbler =
    Gaze.map (Nibblers.repeat newLineNibbler) (fun text -> text |> List.concat |> implode |> Token.NewLine)

let commentNibbler =
    Nibblers.takeAll [ Nibblers.takeString "--"; Nibblers.takeUntil newLineNibbler ] //TODO doesn't handle \r\n

let whiteSpaceNibbler =
    Gaze.map (Nibblers.repeat (Nibblers.take ' ')) (fun ws -> ws |> implode |> Token.WhiteSpace)

let nameOrKeyidentifierTokenNibbler =
    Gaze.map nameNibbler (fun chars -> chars |> List.concat |> implode |> Token.Name)

let tokenNibbler =
    Nibblers.optional (
        Nibblers.repeat (
            Nibblers.takeFirst (
                [ whiteSpaceNibbler
                  nameOrKeyidentifierTokenNibbler
                  integerTokenNibbler
                  newLineTokenNibbler
                  slotTokenNibbler
                  networkNameTokenNibbler
                  stringLiteralTokenNibbler
                  takeAndMap "," Token.Comma
                  takeAndMap "{" Token.OpenBrace
                  takeAndMap "}" Token.CloseBrace
                  takeAndMap "[" Token.OpenSquare
                  takeAndMap "]" Token.CloseSquare ]
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
