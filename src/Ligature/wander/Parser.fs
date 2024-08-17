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
    | Name of string
    | Quote of Element list
    | String of string
    | Int of bigint
    | Bytes of byte array
    | Slot of Slot
    | Call of string * (string * Element) list
    | Network of (Element * Element * Element) list

let nameStrNibbler (gaze: Gaze.Gaze<Token>) : Result<string, Gaze.GazeError> =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.Name(value)) -> Ok(value)
            | _ -> Error Gaze.GazeError.NoMatch)
        gaze

let identifierNib (gaze: Gaze.Gaze<Token>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.Name(name)) -> Ok(Element.Name(name))
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let callNib (gaze: Gaze.Gaze<Token>) =
    result {
        let! identifier = Gaze.attempt (identifierNib) gaze

        match identifier with
        | Element.Name(identifier) -> return Element.Call(identifier, [])
        | _ -> return failwith "TODO"
    }

let readInteger (gaze: Gaze.Gaze<Token>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.Int(i)) -> Ok(Element.Int(i))
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let quoteNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenSquare) gaze
        let! values = Gaze.attempt (optional (repeat elementNib)) gaze
        let! _ = Gaze.attempt (take Token.CloseSquare) gaze
        return Element.Quote(values)
    }

// let quoteNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
//     result {
//         let! _ = Gaze.attempt (take Token.OpenSquare) gaze
//         let! values = (optional (repeat elementNib)) gaze
//         let! _ = Gaze.attempt (take Token.CloseSquare) gaze
//         return Element.Quote(values)
//     }

let statementNib (gaze: Gaze.Gaze<Token>) : Result<(Element * Element * Element), Gaze.GazeError> =
    let entity = patternNib gaze
    let attribute = patternNib gaze

    let value =
        match Gaze.check valueNib gaze with
        | Ok(_) -> valueNib gaze
        | Error(_) -> quoteNib gaze

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
    | Ok(Token.Name(value)) -> Ok(Element.Name value)
    | Ok(Token.Slot(value)) -> Ok(Element.Slot(value))
    | _ -> Error(Gaze.GazeError.NoMatch)

let atomicValueNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Int(value)) -> Ok(Element.Int value)
    | Ok(Token.Name(value)) -> Ok(Element.Name value)
    | Ok(Token.Slot(value)) -> Ok(Element.Slot(value))
    | Ok(Token.StringLiteral(value)) -> Ok(Element.String value)
    | _ -> Error(Gaze.GazeError.NoMatch)

let valueNib = takeFirst [ atomicValueNib; networkNib ]

let rec readValueList (elements: Element list) (gaze: Gaze.Gaze<Token>) : Result<Element list, Gaze.GazeError> =
    let next = Gaze.next gaze

    if next = Ok Token.CloseSquare then
        Ok elements
    else
        let elements =
            match next with
            | Ok(Token.Name w) -> List.append elements [ (Element.Name w) ]
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

let readName (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Name(value)) -> Ok(Element.Name value)
    | _ -> Error(Gaze.GazeError.NoMatch)

let readSlot (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Slot(value)) -> Ok(Element.Slot value)
    | _ -> Error(Gaze.GazeError.NoMatch)

//let patternMatchBodyNib = takeFirst [ networkNib; identifierNib; quoteNib ]

//let patternNib = takeFirst [ networkNib ]

let elementNib = takeFirst [ callNib; networkNib ]

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
    | Element.Quote p -> handleQuote p
    | Element.Slot s -> LigatureValue.Slot s
    | Element.String s -> LigatureValue.String s
    | Element.Name i -> LigatureValue.Name(Name i)
    | Element.Call(i, _) -> failwith "Error?" //LigatureValue.Name(Name i) //TODO I don't think this should ever be called?

let handleQuote (quote: Element list) : LigatureValue =
    List.map (fun element -> elementToValue element) quote |> LigatureValue.Quote

let handleNetwork (network: (Element * Element * Element) list) : Network =
    let res: Set<Statement> = (List.map (elementTupleToStatement) network) |> Set.ofSeq
    res

let elementTupleToStatement ((e, a, v): (Element * Element * Element)) : (PatternName * PatternName * LigatureValue) =
    let entity =
        match e with
        | Element.Name i -> PatternName.Name(Name i)
        | Element.Slot s -> PatternName.Slot s
        | _ -> failwith "TODO"

    let attribute =
        match a with
        | Element.Name i -> PatternName.Name(Name i)
        | Element.Slot s -> PatternName.Slot s
        | _ -> failwith "TODO"

    let value =
        match v with
        | Element.Name i -> LigatureValue.Name(Name i)
        | Element.Int i -> LigatureValue.Int i
        | Element.String s -> LigatureValue.String s
        | Element.Slot s -> LigatureValue.Slot s
        | Element.Quote q -> handleQuote q
        | Element.Network n -> LigatureValue.Network(handleNetwork n)

    (entity, attribute, value)

let rec express (elements: Element list) (expressions: Expression list) : Expression list =
    match elements with
    | [] -> expressions
    | head :: tail ->
        match head with
        | Element.Network n -> express tail (List.append expressions [ Expression.Network(handleNetwork n) ])
        // | Element.Name w ->
        //     express tail (List.append expressions [ Expression.Call(Name(w), { parameterNames = []; quote = [] }) ])
        | Element.Call(i, _) ->
            //            List.map (fun x -> express x) q
            express tail (List.append expressions [ Expression.Call(Name(i), []) ])
        | _ -> failwith "TODO"
