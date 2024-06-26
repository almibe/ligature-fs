// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.LexerUtil

open System.Text.RegularExpressions
open Ligature.Main
open Nibblers
open Fable.Core.JsInterop

let identifierPattern =
    Regex("^[-a-zA-Z0-9._~:/?#\\[\\]@!$&'()*+,;%=]$", RegexOptions.Compiled)

let identifierCharacterNibbler =
    Nibblers.takeWhile (fun c -> identifierPattern.IsMatch(c.ToString()))

let identifierNibbler = Nibblers.between '`' identifierCharacterNibbler '`'

let slotPattern = Regex("^[a-zA-Z0-9_]$", RegexOptions.Compiled)

let slotCharacterNibbler =
    Nibblers.takeWhile (fun c -> slotPattern.IsMatch(c.ToString()))

let slotNibbler =
    Nibblers.takeAllFlatten
        [ Nibblers.takeAll [ Nibblers.take '$' ]
          Nibblers.optional slotCharacterNibbler ]

let parseString (input: string) =
#if !FABLE_COMPILER
    System.Text.Json.Nodes.JsonNode.Parse(input)
#else
    emitJsExpr (input) "JSON.parse($0)"
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
//Int64.fromNumber(unwrap(Number.parse(String.implode(Array.fromList(List.flatten(i)))))))

//let integerValueNibbler = Gaze.map integerNibbler (fun i -> Integer(i))

let bytesNibbler =
    Nibblers.takeAll
        [ Nibblers.takeString "0x"
          Nibblers.takeWhile (fun c -> charInRange c '0' '9' || charInRange c 'A' 'F') ]

let whitespaceNibbler = Nibblers.takeWhile (fun c -> c = ' ' || c = '\t')

let newLineNibbler = Nibblers.takeWhile (fun c -> c = '\n' || c = '\r')

/// <summary>Reads an Identifier in lig format from a Gaze of chars.</summary>
/// <param name="gaze">An instance of Gaze of chars.</param>
/// <returns>An Identifier or Error.</returns>
let readIdentifier gaze =
    match Gaze.attempt identifierNibbler gaze with
    | Error(_) -> error "Could not read Identifier." None
    | Ok(result) -> identifier (System.String(List.toArray result))
