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
type Element =
    | Identifier of string
    | NetworkName of string
    | Pipeline of Element list
    | String of string
    | Int of bigint
    | Bytes of byte array
    | Slot of Slot
    | Call of string
    | Network of (Element * Element * Element) list

let nameStrNibbler (gaze: Gaze.Gaze<Token>) : Result<string, Gaze.GazeError> =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.Identifier(value)) -> Ok(value)
            | _ -> Error Gaze.GazeError.NoMatch)
        gaze

let identifierNib (gaze: Gaze.Gaze<Token>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.Identifier(name)) -> Ok(Element.Identifier(name))
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let networkNameNib (gaze: Gaze.Gaze<Token>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.NetworkName(name)) -> Ok(Element.NetworkName(name))
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let callNib (gaze: Gaze.Gaze<Token>) =
    result {
        let! identifier = Gaze.attempt (identifierNib) gaze

        match identifier with
        | Element.Identifier(identifier) -> return Element.Call(identifier)
        | _ -> return failwith "TODO"
    }

let readInteger (gaze: Gaze.Gaze<Token>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.Int(i)) -> Ok(Element.Int(i))
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let pipelineNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenSquare) gaze
        let! values = Gaze.attempt (optional (repeat elementNib)) gaze
        let! _ = Gaze.attempt (take Token.CloseSquare) gaze
        return Element.Pipeline(values)
    }

// let pipelineNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
//     result {
//         let! _ = Gaze.attempt (take Token.OpenSquare) gaze
//         let! values = (optional (repeat elementNib)) gaze
//         let! _ = Gaze.attempt (take Token.CloseSquare) gaze
//         return Element.Pipeline(values)
//     }

let statementNib (gaze: Gaze.Gaze<Token>) : Result<(Element * Element * Element), Gaze.GazeError> =
    let entity = patternNib gaze
    let attribute = patternNib gaze

    let value =
        match Gaze.check valueNib gaze with
        | Ok(_) -> valueNib gaze
        | Error(_) -> pipelineNib gaze

    match (entity, attribute, value) with
    | (Ok(e), Ok(a), Ok(v)) -> Ok(e, a, v)
    | _ -> Error(Gaze.NoMatch)

let networkNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenBrace) gaze
        let! statements = (optional (repeatSep statementNib Token.Comma)) gaze
        let! _ = Gaze.attempt (take Token.CloseBrace) gaze
        return Element.Network(statements)
    }

let patternNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    match Gaze.next gaze with
    | Error(err) -> Error err
    | Ok(Token.Identifier(value)) -> Ok(Element.Identifier value)
    | Ok(Token.Slot(value)) -> Ok(Element.Slot(value))
    | _ -> Error(Gaze.GazeError.NoMatch)

/// Read the next Element from the given instance of Gaze<Token>
let valueNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Int(value)) -> Ok(Element.Int value)
    | Ok(Token.Identifier(value)) -> Ok(Element.Identifier value)
    | Ok(Token.Slot(value)) -> Ok(Element.Slot(value))
    | Ok(Token.NetworkName(name)) -> Ok(Element.NetworkName(name))
    | Ok(Token.StringLiteral(value)) -> Ok(Element.String value)
    | _ -> Error(Gaze.GazeError.NoMatch)

let rec readValueList (elements: Element list) (gaze: Gaze.Gaze<Token>) : Result<Element list, Gaze.GazeError> =
    let next = Gaze.next gaze

    if next = Ok Token.CloseSquare then
        Ok elements
    else
        let elements =
            match next with
            | Ok(Token.Identifier w) -> List.append elements [ (Element.Identifier w) ]
            | Ok(Token.StringLiteral s) -> List.append elements [ (Element.String s) ]
            | Ok(Token.Int i) -> List.append elements [ (Element.Int i) ]
            | Ok(Token.Slot s) -> List.append elements [ (Element.Slot s) ]
            | _ -> failwith "TODO"

        match Gaze.peek gaze with
        | Ok Token.CloseSquare ->
            (Gaze.next gaze |> ignore)
            Ok elements
        | Ok Token.Comma ->
            (Gaze.next gaze |> ignore)
            readValueList elements gaze
        | _ -> failwith "TODO"

let readIdentifier (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Identifier(value)) -> Ok(Element.Identifier value)
    | _ -> Error(Gaze.GazeError.NoMatch)

let readSlot (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Slot(value)) -> Ok(Element.Slot value)
    | _ -> Error(Gaze.GazeError.NoMatch)

//let patternMatchBodyNib = takeFirst [ networkNib; identifierNib; pipelineNib ]

//let patternNib = takeFirst [ networkNib ]

let elementNib = takeFirst [ networkNameNib; callNib; networkNib ]

let scriptNib = repeat elementNib

/// <summary></summary>
/// <param name="tokens">The list of Tokens to be parsered.</param>
/// <returns>The AST created from the token list of an Error.</returns>
let parse (tokens: Token list) : Result<Element list, LigatureError> =
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

let elementToValue (element: Element) : LigatureValue =
    match element with
    | Element.Int i -> LigatureValue.Int i
    | Element.Bytes b -> LigatureValue.Bytes b
    | Element.Network n -> LigatureValue.Network(handleNetwork n)
    | Element.Pipeline p -> handlePipeline p
    | Element.Slot s -> LigatureValue.Slot s
    | Element.String s -> LigatureValue.String s
    | Element.Identifier i -> LigatureValue.Identifier(Identifier i)
    | Element.Call i -> LigatureValue.Identifier(Identifier i)
    | Element.NetworkName n -> LigatureValue.NetworkName n

let handlePipeline (pipeline: Element list) : LigatureValue =
    List.map (fun element -> elementToValue element) pipeline |> LigatureValue.Pipeline

let elementTupleToStatement
    ((e, a, v): (Element * Element * Element))
    : (PatternIdentifier * PatternIdentifier * LigatureValue) =
    let entity =
        match e with
        | Element.Identifier i -> PatternIdentifier.Identifier(Identifier i)
        | Element.Slot s -> PatternIdentifier.Slot s
        | _ -> failwith "TODO"

    let attribute =
        match a with
        | Element.Identifier i -> PatternIdentifier.Identifier(Identifier i)
        | Element.Slot s -> PatternIdentifier.Slot s
        | _ -> failwith "TODO"

    let value =
        match v with
        | Element.Identifier i -> LigatureValue.Identifier(Identifier i)
        | Element.Int i -> LigatureValue.Int i
        | Element.String s -> LigatureValue.String s
        | Element.Slot s -> LigatureValue.Slot s
        | Element.Pipeline(q) -> handlePipeline q
        | Element.NetworkName n -> LigatureValue.NetworkName n
        | _ -> failwith "TODO"

    (entity, attribute, value)

let handleNetwork (network: (Element * Element * Element) list) : Network =
    let res: Set<Statement> = (List.map (elementTupleToStatement) network) |> Set.ofSeq

    res

let rec express (elements: Element list) (expressions: Expression list) : Expression list =
    match elements with
    | [] -> expressions
    | head :: tail ->
        match head with
        | Element.Network n -> express tail (List.append expressions [ Expression.Network(handleNetwork n) ])
        // | Element.Identifier w ->
        //     express tail (List.append expressions [ Expression.Call(Identifier(w), { parameterNames = []; pipeline = [] }) ])
        | Element.Call i ->
            //            List.map (fun x -> express x) q
            express tail (List.append expressions [ Expression.Call(Identifier(i)) ])
        | Element.NetworkName name -> express tail (List.append expressions [ Expression.NetworkName name ])
        | _ -> failwith "TODO"
