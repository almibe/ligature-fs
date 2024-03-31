// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Lig.Read

open System.Text.RegularExpressions
open Ligature

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

let identifierPattern =
    Regex("^[-a-zA-Z0-9._~:/?#\\[\\]@!$&'()*+,;%=]$", RegexOptions.Compiled)

let identifierCharacterNibbler =
    Nibblers.takeWhile (fun c -> identifierPattern.IsMatch(c.ToString()))

let identifierNibbler = Nibblers.between '<' identifierCharacterNibbler '>'

//TODO the below Nibbler is incorrect
let stringContentNibbler =
    Nibblers.takeWhile (fun c -> identifierPattern.IsMatch(c.ToString()))

/// A Nibbler that reads Strings as defined by lig.
/// TODO: this parser is incomplete and just used for testing currently.
let stringNibbler =
    Gaze.map (Nibblers.between '"' stringContentNibbler '"') (fun c -> System.String(c |> List.toArray))

let stringValueNibbler = Gaze.map stringNibbler (fun s -> Value.String(s))

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

let integerValueNibbler = Gaze.map integerNibbler (fun i -> Value.Integer(i))

let bytesNibbler =
    Nibblers.takeAll
        [ Nibblers.takeString "0x"
          Nibblers.takeWhile (fun c -> charInRange c '0' '9' || charInRange c 'A' 'F') ]

//TODO add bytes nibbler
//let valueNibbler = Nibblers.takeFirst [stringValueNibbler integerValueNibbler]
let valueNibbler = stringValueNibbler

let whitespaceNibbler = Nibblers.takeWhile (fun c -> c = ' ' || c = '\t')

let newLineNibbler = Nibblers.takeWhile (fun c -> c = '\n' || c = '\r')

/// <summary>Reads an Identifier in lig format from a Gaze of chars.</summary>
/// <param name="gaze">An instance of Gaze of chars.</param>
/// <returns>An Identifier or Error.</returns>
let readIdentifier gaze =
    match Gaze.attempt identifierNibbler gaze with
    | Error(_) -> error "Could not read Identifier." None
    | Ok(result) -> identifier (System.String(List.toArray result))

/// <summary>Reads a Value in lig format from a Gaze of chars.
/// This could be an Identifier, String, Byte Array, or Integer.</summary>
/// <param name="gaze">An instance of Gaze of chars.</param>
/// <returns>A lig Value or an Error.</returns>
let readValue gaze =
    let id = readIdentifier gaze

    match id with
    | Ok(i) -> Ok(Value.Identifier(i))
    | Error(_) ->
        match (Gaze.attempt valueNibbler gaze) with
        | Error _ -> error "Could not read Value." None
        | Ok result -> Ok(result)

//TODO remove need for unwrap function
let unwrap result =
    match result with
    | Ok(result) -> result
    | Error _ -> todo

/// <summary>Reads in a String and returns a List of Statements or an Error.</summary>
/// <param name="lig">The input String in lig format.</param>
/// <returns>A List of Statements or an Error.</returns>
let readLig (lig: string) =
    let gaze = Gaze.fromString lig
    let mutable cont = true
    let mutable statements = []

    //read opening white space
    Gaze.attempt (Nibblers.repeat (Nibblers.takeWhile (fun c -> c = ' ' || c = '\t' || c = '\n' || c = '\r'))) gaze
    |> ignore

    while (not (Gaze.isComplete gaze)) && cont do
        let entity = readIdentifier gaze
        Gaze.attempt whitespaceNibbler gaze |> ignore //TODO don't ignore
        let attribute = readIdentifier gaze
        Gaze.attempt whitespaceNibbler gaze |> ignore //TODO don't ignore
        let value: Result<Value, _> = readValue gaze

        Gaze.attempt (Nibblers.repeat (Nibblers.takeWhile (fun c -> c = ' ' || c = '\t' || c = '\n' || c = '\r'))) gaze
        |> ignore

        match (entity, attribute, value) with
        | ((Ok entity), (Ok attribute), (Ok value)) ->
            let statement: Statement =
                { Entity = entity
                  Attribute = attribute
                  Value = value }
            statements <- List.append statements [ statement ]
        | _ -> ()
    
    if cont then
        Ok(statements)
    else
        Error("Could not read Lig.")
