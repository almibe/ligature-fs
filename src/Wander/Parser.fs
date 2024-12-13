// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module rec Wander.Parser

open Tokenizer
open FsToolkit.ErrorHandling
open Nibblers
open Ligature.Main
open Model

let identifierNib (gaze: Gaze.Gaze<Token>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.Element(name)) -> Ok(Value.Element(Element name))
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

//let elementNib = takeFirst [ quoteNib; networkNib ]

let elementNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Element(value)) -> Ok(Element value)
    | _ -> Error(Gaze.GazeError.NoMatch)

let quoteNib (gaze: Gaze.Gaze<Token>) : Result<Any, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenParen) gaze
        let! values = Gaze.attempt (optional (repeat anyNib)) gaze
        let! _ = Gaze.attempt (take Token.CloseParen) gaze
        return Any.Quote values
    }

let statementNib (gaze: Gaze.Gaze<Token>) : Result<(Element * Element * Value), Gaze.GazeError> =
    let entity = symbolNib gaze
    let attribute = symbolNib gaze
    let value = Gaze.attempt valueNib gaze

    match (entity, attribute, value) with
    | (Ok(e), Ok(a), Ok(v)) -> Ok(e, a, v)
    | _ -> Error(Gaze.NoMatch)

let networkNib (gaze: Gaze.Gaze<Token>) : Result<Any, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenBrace) gaze
        let! statements = (optional (repeatSep statementNib Token.Comma)) gaze
        let! _ = Gaze.attempt (take Token.CloseBrace) gaze
        return Any.Network(expressNetwork statements)
    }

let symbolNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Element(value)) -> Ok(Element value)
    | Ok(Token.StringLiteral(value)) -> Ok(Element value)
    | _ -> Error(Gaze.GazeError.NoMatch)

let elementOrLiteralNib (gaze: Gaze.Gaze<Token>) : Result<Any, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Ok(Token.Element(value)) -> 
        if value.StartsWith "?" then
            Ok(Any.Variable(Variable value))
        else
            Ok(Any.Element(Element value))
    | Ok(Token.StringLiteral(value)) -> Ok(Any.Literal value)
    | _ -> Error(Gaze.GazeError.NoMatch)

let anyNib: Gaze.Nibbler<Token, Any> =
    takeFirst [ quoteNib; elementOrLiteralNib; networkNib ]

let valueNib (gaze: Gaze.Gaze<Token>) : Result<Value, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Ok(Token.Element(value)) -> 
        // if value.StartsWith "?" then
        //     Ok(Value.Variable(Variable value))
        // else
            Ok(Value.Element(Element value))
    | Ok(Token.StringLiteral(value)) -> Ok(Value.Literal value)
    | _ -> Error(Gaze.GazeError.NoMatch)

let callNib (gaze: Gaze.Gaze<Token>) : Result<Call, Gaze.GazeError> =
    result {
        let! name = Gaze.attempt elementNib gaze
        let! arguments = Gaze.attempt (optional (repeat anyNib)) gaze
        return name, arguments
    }

let scriptNib = repeatSep callNib Token.Comma

/// <summary></summary>
/// <param name="tokens">The list of Tokens to be parsered.</param>
/// <returns>The AST created from the token list of an Error.</returns>
let parse (tokens: Token list) : Result<Call list, LigatureError> =
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
                //failwith "TODO"
                Ok res
            else
                error $"Failed to parse completely. {Gaze.remaining gaze}" None
        | Error err -> error $"Failed to parse.\n{err.ToString()}" None

/// <summary></summary>
/// <param name="tokens">The list of Tokens to be parsered.</param>
/// <returns>The AST created from the token list of an Error.</returns>
let read (tokens: Token list) : Result<Any, LigatureError> =
    let tokens =
        List.filter
            (fun token ->
                match token with
                | Token.WhiteSpace(_)
                | Token.NewLine(_) -> false
                | _ -> true)
            tokens

    if tokens.IsEmpty then
        error "Illegal call to read." None
    else
        let gaze = Gaze.fromList tokens

        match Gaze.attempt anyNib gaze with
        | Ok res ->
            if Gaze.isComplete gaze then
                //failwith "TODO"
                Ok res
            else
                error $"Failed to read completely. {Gaze.remaining gaze}" None
        | Error err -> error $"Failed to parse.\n{err.ToString()}" None

/// Helper function that handles tokienization for you.
let parseString (input: string) =
    match tokenize input with
    | Ok tokens -> parse tokens
    | Error err -> error "Could not parse input." None //error $"Could not match from {gaze.offset} - {(Gaze.remaining gaze)}." None //TODO this error message needs updated

let expressNetwork (network: (Element * Element * Value) list) : Set<Entry> =
    let res: Set<Entry> = (List.map (elementTupleToEntry) network) |> Set.ofSeq
    res

let elementTupleToEntry (tuple: (Element * Element * Value)) : Entry =
    match tuple with
    | (element, Element ":", Value.Element concept) -> Entry.Extends { element = element; concept = concept }
    | (element, Element "¬:", Value.Element concept) -> Entry.NotExtends { element = element; concept = concept }
    | (element, attribute, value) ->
        Entry.Attribute
            { element = element
              attribute = attribute
              value = value }
