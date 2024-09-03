// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module rec Ligature.Wander.Parser

open Lexer
open FsToolkit.ErrorHandling
open Model
open Nibblers
open Ligature.Main

// [<RequireQualifiedAccess>]
// type LigatureValue =
//     | Name of string
//     | NetworkName of string
//     | Quote of LigatureValue list
//     | Expression of LigatureValue list
//     | String of string
//     | Int of bigint
//     | Bytes of byte array
//     | Slot of Slot
//     | Network of (LigatureValue * LigatureValue * LigatureValue) list

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
            | Ok(Token.Name(name)) -> Ok(LigatureValue.Name(Name(name)))
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let networkNameNib (gaze: Gaze.Gaze<Token>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.NetworkName(name)) -> Ok(Command.NetworkName(NetworkName(name)))
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

// let readNameStr (gaze: Gaze.Gaze<Token>) : Result<string, Gaze.GazeError> =
//     let next = Gaze.next gaze

//     match next with
//     | Error(err) -> Error err
//     | Ok(Token.Name(value)) -> Ok value
//     | _ -> Error(Gaze.GazeError.NoMatch)

let readName (gaze: Gaze.Gaze<Token>) : Result<LigatureValue, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Name(value)) -> Ok(LigatureValue.Name(Name(value)))
    | _ -> Error(Gaze.GazeError.NoMatch)

let readNetworkName (gaze: Gaze.Gaze<Token>) : Result<LigatureValue, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.NetworkName(value)) -> Ok(LigatureValue.NetworkName(NetworkName(value)))
    | _ -> Error(Gaze.GazeError.NoMatch)

let readInteger (gaze: Gaze.Gaze<Token>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.Int(i)) -> Ok(LigatureValue.Int(i))
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let quoteNib (gaze: Gaze.Gaze<Token>) : Result<LigatureValue, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenSquare) gaze
        let! values = Gaze.attempt (optional (repeat elementNib)) gaze
        let! _ = Gaze.attempt (take Token.CloseSquare) gaze
        return LigatureValue.Quote(values)
    }

let expressionNib (gaze: Gaze.Gaze<Token>) : Result<LigatureValue, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenParen) gaze
        let! values = Gaze.attempt (optional (repeat valueNib)) gaze
        let! _ = Gaze.attempt (take Token.CloseParen) gaze
        return LigatureValue.Expression(values)
    }

// let argumentNib (gaze: Gaze.Gaze<Token>) : Result<(string * LigatureValue), Gaze.GazeError> =
//     let entity = patternNib gaze
//     let attribute = patternNib gaze

//     let value =
//         match Gaze.check valueNib gaze with
//         | Ok(_) -> valueNib gaze
//         | Error(_) -> quoteNib gaze

//     match (entity, attribute, value) with
//     | (Ok(LigatureValue.Name(name)), Ok(a), Ok(v)) -> Ok(name, v)
//     | _ -> Error(Gaze.NoMatch)

let statementNib (gaze: Gaze.Gaze<Token>) : Result<(LigatureValue * LigatureValue * LigatureValue), Gaze.GazeError> =
    let entity = patternNib gaze
    let attribute = patternNib gaze

    let value =
        match Gaze.check valueNib gaze with
        | Ok(_) -> valueNib gaze
        | Error(_) -> quoteNib gaze

    match (entity, attribute, value) with
    | (Ok(e), Ok(a), Ok(v)) -> Ok(e, a, v)
    | _ -> Error(Gaze.NoMatch)

let networkNib (gaze: Gaze.Gaze<Token>) : Result<LigatureValue, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenBrace) gaze
        let! statements = (optional (repeatSep statementNib Token.Comma)) gaze
        let! _ = Gaze.attempt (take Token.CloseBrace) gaze
        return LigatureValue.Network(handleNetwork statements)
    }

let patternNib (gaze: Gaze.Gaze<Token>) : Result<LigatureValue, Gaze.GazeError> =
    match Gaze.next gaze with
    | Error(err) -> Error err
    | Ok(Token.Name(value)) -> Ok(LigatureValue.Name(Name(value)))
    | Ok(Token.Slot(value)) -> Ok(LigatureValue.Slot(value))
    | _ -> Error(Gaze.GazeError.NoMatch)

let atomicValueNib (gaze: Gaze.Gaze<Token>) : Result<LigatureValue, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Int(value)) -> Ok(LigatureValue.Int value)
    | Ok(Token.Name(value)) -> Ok(LigatureValue.Name(Name(value)))
    | Ok(Token.Slot(value)) -> Ok(LigatureValue.Slot(value))
    | Ok(Token.NetworkName(value)) -> Ok(LigatureValue.NetworkName(NetworkName(value)))
    | Ok(Token.StringLiteral(value)) -> Ok(LigatureValue.String value)
    | _ -> Error(Gaze.GazeError.NoMatch)

let valueNib = takeFirst [ quoteNib; expressionNib; atomicValueNib; networkNib ]

let rec readValueList
    (elements: LigatureValue list)
    (gaze: Gaze.Gaze<Token>)
    : Result<LigatureValue list, Gaze.GazeError> =
    let next = Gaze.next gaze

    if next = Ok Token.CloseSquare then
        Ok elements
    else
        let elements =
            match next with
            | Ok(Token.Name w) -> List.append elements [ (LigatureValue.Name(Name(w))) ]
            | Ok(Token.StringLiteral s) -> List.append elements [ (LigatureValue.String s) ]
            | Ok(Token.Int i) -> List.append elements [ (LigatureValue.Int i) ]
            | Ok(Token.Slot s) -> List.append elements [ (LigatureValue.Slot s) ]

        match Gaze.peek gaze with
        | Ok Token.CloseSquare ->
            (Gaze.next gaze |> ignore)
            Ok elements
        | Ok Token.Comma ->
            (Gaze.next gaze |> ignore)
            readValueList elements gaze

let readSlot (gaze: Gaze.Gaze<Token>) : Result<LigatureValue, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Slot(value)) -> Ok(LigatureValue.Slot value)
    | _ -> Error(Gaze.GazeError.NoMatch)

//let patternMatchBodyNib = takeFirst [ networkNib; identifierNib; quoteNib ]

//let patternNib = takeFirst [ networkNib ]

let elementNib = takeFirst [ expressionNib; networkNib; readNetworkName ]

let scriptNib = repeat valueNib

/// <summary></summary>
/// <param name="tokens">The list of Tokens to be parsered.</param>
/// <returns>The AST created from the token list of an Error.</returns>
let parse (tokens: Token list) : Result<LigatureValue list, LigatureError> =
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

// let elementToValue (element: LigatureValue) : LigatureValue =
//     match element with
//     | LigatureValue.Int i -> LigatureValue.Int i
//     | LigatureValue.Bytes b -> LigatureValue.Bytes b
//     | LigatureValue.Network n -> LigatureValue.Network(handleNetwork n)
//     | LigatureValue.Quote p -> handleQuote p
//     | LigatureValue.Expression e -> handleExpression e
//     | LigatureValue.Slot s -> LigatureValue.Slot s
//     | LigatureValue.String s -> LigatureValue.String s
//     | LigatureValue.Name n -> LigatureValue.Name(Name n)
//     | LigatureValue.NetworkName n -> LigatureValue.NetworkName(NetworkName(n))

// let handleQuote (quote: LigatureValue list) : LigatureValue =
//     List.map (fun element -> elementToValue element) quote |> LigatureValue.Quote

// let handleExpression (expression: LigatureValue list) : LigatureValue =
//     List.map (fun element -> elementToValue element) expression
//     |> LigatureValue.Expression

let handleNetwork (network: (LigatureValue * LigatureValue * LigatureValue) list) : Network =
    let res: Set<Statement> = (List.map (elementTupleToStatement) network) |> Set.ofSeq
    res

let elementTupleToStatement
    ((e, a, v): (LigatureValue * LigatureValue * LigatureValue))
    : (PatternName * PatternName * LigatureValue) =
    let entity =
        match e with
        | LigatureValue.Name p -> PatternName.Name p
        | LigatureValue.Slot s -> PatternName.Slot s
        | _ -> failwith "Error - unexpected Entity."

    let attribute =
        match a with
        | LigatureValue.Name p -> PatternName.Name p
        | LigatureValue.Slot s -> PatternName.Slot s
        | _ -> failwith "Error - unexpected Attribute."

    let value = v

    (entity, attribute, value)

let expressExpression (elements: LigatureValue list) : Command =
    //    let res = List.map (fun element -> elementToValue element) elements
    Command.Expression elements

let rec express (elements: LigatureValue list) (expressions: Command list) : Command list =
    match elements with
    | [] -> expressions
    | head :: tail ->
        match head with
        | LigatureValue.Network n -> express tail (List.append expressions [ Command.Network n ])
        | LigatureValue.Expression e -> express tail (List.append expressions [ expressExpression e ])
        | LigatureValue.NetworkName n -> express tail (List.append expressions [ Command.NetworkName n ])
        | _ -> failwith "Error - unexpected token."
