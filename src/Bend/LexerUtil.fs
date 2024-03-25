// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Bend.LexerUtil

open System.Text.RegularExpressions
open Error
open Ligature

let identifierPattern =
    Regex("^[-a-zA-Z0-9._~:/?#\\[\\]@!$&'()*+,;%=]$", RegexOptions.Compiled)

let identifierCharacterNibbler =
    Nibblers.takeWhile (fun c -> identifierPattern.IsMatch(c.ToString()))

let identifierNibbler = Nibblers.between '`' identifierCharacterNibbler '`'

//TODO the below Nibbler is incorrect
let stringContentNibbler =
    Nibblers.takeWhile (fun c -> Regex("^[-a-zA-Z0-9._~:/?#\\[\\]@!$&'()*+,;%= ]$", RegexOptions.Compiled).IsMatch(c.ToString()))

/// A Nibbler that reads Strings as defined by lig.
/// TODO: this parser is incomplete and just used for testing currently.
let stringNibbler =
    Gaze.map (Nibblers.between '"' stringContentNibbler '"') (fun c -> System.String(c |> List.toArray))

//let stringTokenNibbler = Gaze.map stringNibbler (fun s -> Token.StringLiteral(s))

let charInRange char start stop = char >= start && char <= stop

let charListToInt i =
    int64 (System.String(List.concat i |> List.toArray))

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
