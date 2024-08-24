// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module rec Ligature.Wander.Parser

open Lexer
open FsToolkit.ErrorHandling
open Model
open Nibblers
open Ligature.Main

[<RequireQualifiedAccess>]
type ParserElement =
    | Name of string
    | NetworkName of string
    | Pipeline of ParserElement list
    | String of string
    | Int of bigint
    | Bytes of byte array
    | Slot of Slot
    | Network of (ParserElement * ParserElement * ParserElement) list

// let nameStrNibbler (gaze: Gaze.Gaze<Token>) : Result<string, Gaze.GazeError> =
//     Gaze.attempt
//         (fun gaze ->
//             match Gaze.next gaze with
//             | Ok(Token.Name(value)) -> Ok(value)
//             | _ -> Error Gaze.GazeError.NoMatch)
//         gaze

let identifierNib (gaze: Gaze.Gaze<Token>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.Name(name)) -> Ok(ParserElement.Name(name))
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let networkNameNib (gaze: Gaze.Gaze<Token>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.NetworkName(name)) -> Ok(Element.NetworkName(NetworkName(name)))
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

// let readNameStr (gaze: Gaze.Gaze<Token>) : Result<string, Gaze.GazeError> =
//     let next = Gaze.next gaze

//     match next with
//     | Error(err) -> Error err
//     | Ok(Token.Name(value)) -> Ok value
//     | _ -> Error(Gaze.GazeError.NoMatch)

let readName (gaze: Gaze.Gaze<Token>) : Result<ParserElement, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Name(value)) -> Ok(ParserElement.Name value)
    | _ -> Error(Gaze.GazeError.NoMatch)

let readNetworkName (gaze: Gaze.Gaze<Token>) : Result<ParserElement, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.NetworkName(value)) -> Ok(ParserElement.NetworkName value)
    | _ -> Error(Gaze.GazeError.NoMatch)

let readInteger (gaze: Gaze.Gaze<Token>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.Int(i)) -> Ok(ParserElement.Int(i))
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let quoteNib (gaze: Gaze.Gaze<Token>) : Result<ParserElement, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenSquare) gaze
        let! values = Gaze.attempt (optional (repeat elementNib)) gaze
        let! _ = Gaze.attempt (take Token.CloseSquare) gaze
        return ParserElement.Pipeline(values)
    }

// let argumentNib (gaze: Gaze.Gaze<Token>) : Result<(string * ParserElement), Gaze.GazeError> =
//     let entity = patternNib gaze
//     let attribute = patternNib gaze

//     let value =
//         match Gaze.check valueNib gaze with
//         | Ok(_) -> valueNib gaze
//         | Error(_) -> quoteNib gaze

//     match (entity, attribute, value) with
//     | (Ok(ParserElement.Name(name)), Ok(a), Ok(v)) -> Ok(name, v)
//     | _ -> Error(Gaze.NoMatch)

let statementNib (gaze: Gaze.Gaze<Token>) : Result<(ParserElement * ParserElement * ParserElement), Gaze.GazeError> =
    let entity = patternNib gaze
    let attribute = patternNib gaze

    let value =
        match Gaze.check valueNib gaze with
        | Ok(_) -> valueNib gaze
        | Error(_) -> quoteNib gaze

    match (entity, attribute, value) with
    | (Ok(e), Ok(a), Ok(v)) -> Ok(e, a, v)
    | _ -> Error(Gaze.NoMatch)

let networkNib (gaze: Gaze.Gaze<Token>) : Result<ParserElement, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenBrace) gaze
        let! statements = (optional (repeatSep statementNib Token.Comma)) gaze
        let! _ = Gaze.attempt (take Token.CloseBrace) gaze
        return ParserElement.Network(statements)
    }

let patternNib (gaze: Gaze.Gaze<Token>) : Result<ParserElement, Gaze.GazeError> =
    match Gaze.next gaze with
    | Error(err) -> Error err
    | Ok(Token.Name(value)) -> Ok(ParserElement.Name value)
    | Ok(Token.Slot(value)) -> Ok(ParserElement.Slot(value))
    | _ -> Error(Gaze.GazeError.NoMatch)

let atomicValueNib (gaze: Gaze.Gaze<Token>) : Result<ParserElement, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Int(value)) -> Ok(ParserElement.Int value)
    | Ok(Token.Name(value)) -> Ok(ParserElement.Name value)
    | Ok(Token.Slot(value)) -> Ok(ParserElement.Slot(value))
    | Ok(Token.StringLiteral(value)) -> Ok(ParserElement.String value)
    | _ -> Error(Gaze.GazeError.NoMatch)

let valueNib = takeFirst [ atomicValueNib; networkNib ]

let rec readValueList
    (elements: ParserElement list)
    (gaze: Gaze.Gaze<Token>)
    : Result<ParserElement list, Gaze.GazeError> =
    let next = Gaze.next gaze

    if next = Ok Token.CloseSquare then
        Ok elements
    else
        let elements =
            match next with
            | Ok(Token.Name w) -> List.append elements [ (ParserElement.Name w) ]
            | Ok(Token.StringLiteral s) -> List.append elements [ (ParserElement.String s) ]
            | Ok(Token.Int i) -> List.append elements [ (ParserElement.Int i) ]
            | Ok(Token.Slot s) -> List.append elements [ (ParserElement.Slot s) ]
            | _ -> failwith "TODO"

        match Gaze.peek gaze with
        | Ok Token.CloseSquare ->
            (Gaze.next gaze |> ignore)
            Ok elements
        | Ok Token.Comma ->
            (Gaze.next gaze |> ignore)
            readValueList elements gaze
        | _ -> failwith "TODO"

let readSlot (gaze: Gaze.Gaze<Token>) : Result<ParserElement, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Slot(value)) -> Ok(ParserElement.Slot value)
    | _ -> Error(Gaze.GazeError.NoMatch)

//let patternMatchBodyNib = takeFirst [ networkNib; identifierNib; quoteNib ]

//let patternNib = takeFirst [ networkNib ]

let elementNib = takeFirst [ quoteNib; readName; networkNib; readNetworkName ]

let scriptNib = repeat elementNib

/// <summary></summary>
/// <param name="tokens">The list of Tokens to be parsered.</param>
/// <returns>The AST created from the token list of an Error.</returns>
let parse (tokens: Token list) : Result<ParserElement list, LigatureError> =
    let tokens =
        List.filter
            (fun token ->
                match token with
                | Token.WhiteSpace(_)
                | Token.NewLine(_) -> false
                | _ -> true)
            tokens

    if tokens.IsEmpty then
        Ok []
    else
        let gaze = Gaze.fromList tokens

        match Gaze.attempt scriptNib gaze with
        | Ok res ->
            if Gaze.isComplete gaze then
                Ok res
            else
                error $"Failed to parse completely. {Gaze.remaining gaze}" None
        | Error err -> error $"Failed to parse.\n{err.ToString()}" None

/// Helper function that handles tokienization for you.
let parseString (input: string) =
    match tokenize input with
    | Ok tokens -> parse tokens
    | Error err -> error "Could not parse input." None //error $"Could not match from {gaze.offset} - {(Gaze.remaining gaze)}." None //TODO this error message needs updated

let elementToValue (element: ParserElement) : LigatureValue =
    match element with
    | ParserElement.Int i -> LigatureValue.Int i
    | ParserElement.Bytes b -> LigatureValue.Bytes b
    | ParserElement.Network n -> LigatureValue.Network(handleNetwork n)
    | ParserElement.Pipeline p -> handlePipeline p
    | ParserElement.Slot s -> LigatureValue.Slot s
    | ParserElement.String s -> LigatureValue.String s
    | ParserElement.Name p -> failwith "TODO" //LigatureValue.Name(Name i)
    | ParserElement.NetworkName n -> LigatureValue.NetworkName(NetworkName(n))

let handlePipeline (quote: ParserElement list) : LigatureValue = failwith "TODO"
//    List.map (fun element -> elementToValue element) quote |> LigatureValue.Pipeline

let handleNetwork (network: (ParserElement * ParserElement * ParserElement) list) : Network =
    let res: Set<Statement> = (List.map (elementTupleToStatement) network) |> Set.ofSeq
    res

let elementTupleToStatement
    ((e, a, v): (ParserElement * ParserElement * ParserElement))
    : (PatternName * PatternName * LigatureValue) =
    let entity =
        match e with
        | ParserElement.Name p -> PatternName.Name(Name p)
        | ParserElement.Slot s -> PatternName.Slot s
        | _ -> failwith "Error - unexpected Entity."

    let attribute =
        match a with
        | ParserElement.Name p -> PatternName.Name(Name p)
        | ParserElement.Slot s -> PatternName.Slot s
        | _ -> failwith "Error - unexpected Attribute."

    let value =
        match v with
        | ParserElement.Name p -> LigatureValue.Name(Name(p))
        | ParserElement.Int i -> LigatureValue.Int i
        | ParserElement.String s -> LigatureValue.String s
        | ParserElement.Slot s -> LigatureValue.Slot s
        | ParserElement.Pipeline q -> handlePipeline q
        | ParserElement.Network n -> LigatureValue.Network(handleNetwork n)
        | ParserElement.Bytes b -> LigatureValue.Bytes b
        | ParserElement.NetworkName n -> LigatureValue.NetworkName(NetworkName(n))

    (entity, attribute, value)

let expressPipeline (elements: ParserElement list) : Element list = express elements []

let rec express (elements: ParserElement list) (expressions: Element list) : Element list =
    match elements with
    | [] -> expressions
    | head :: tail ->
        match head with
        | ParserElement.Network n -> express tail (List.append expressions [ Element.Network(handleNetwork n) ])
        | ParserElement.Pipeline p -> express tail (List.append expressions (expressPipeline p))
        | ParserElement.Name n -> express tail (List.append expressions [ Element.Name(Name n) ])
        | ParserElement.NetworkName n -> express tail (List.append expressions [ Element.NetworkName(NetworkName n) ])
        | _ -> failwith "Error - unexpected token."
