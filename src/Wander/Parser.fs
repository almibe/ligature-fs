// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module rec Wander.Parser

open Tokenizer
open FsToolkit.ErrorHandling
open Nibblers
open Ligature.Model
open Model

let variableNib (gaze: Gaze.Gaze<Token>) : Result<Variable, Gaze.GazeError> =
    match Gaze.next gaze with
    | Error(err) -> Error err
    | Ok(Token.Variable(value)) -> Ok(Variable value)
    | _ -> Error(Gaze.GazeError.NoMatch)

let quoteNib (gaze: Gaze.Gaze<Token>) : Result<Quote, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenSquare) gaze
        let! values = Gaze.attempt (optional (repeat anyNib)) gaze
        let! _ = Gaze.attempt (take Token.CloseSquare) gaze
        return values
    }

let partialQuoteNib (gaze: Gaze.Gaze<Token>) : Result<Quote, Gaze.GazeError> =
    result {
        let! values = Gaze.attempt (optional (repeat anyNib)) gaze
        let! _ = Gaze.attempt (take Token.CloseSquare) gaze
        return values
    }

let commentNib (gaze: Gaze.Gaze<Token>) : Result<Any, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenParen) gaze
        let! values = Gaze.attempt (optional (repeat anyNib)) gaze
        let! _ = Gaze.attempt (take Token.CloseParen) gaze
        return Any.Comment(values.ToString())
    }

let quoteAnyNib (gaze: Gaze.Gaze<Token>) : Result<Any, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenSquare) gaze
        let! values = Gaze.attempt (optional (repeat anyNib)) gaze
        let! _ = Gaze.attempt (take Token.CloseSquare) gaze
        return Any.Quote values
    }

let patternStatementNib (gaze: Gaze.Gaze<Token>) : Result<(ElementPattern * ElementPattern * Value), Gaze.GazeError> =
    let entity = elementPatternNib gaze
    let attribute = elementPatternNib gaze
    let value = Gaze.attempt valuePatternNib gaze

    match (entity, attribute, value) with
    | (Ok(e), Ok(a), Ok(v)) -> Ok(e, a, v)
    | _ -> Error(Gaze.NoMatch)

let partialNetworkNib (gaze: Gaze.Gaze<Token>) : Result<Network, Gaze.GazeError> =
    result {
        let! statements = (optional (repeatSep patternStatementNib Token.Comma)) gaze
        let! _ = Gaze.attempt (take Token.CloseBrace) gaze
        return Set.ofList statements
    }

let networkNib (gaze: Gaze.Gaze<Token>) : Result<Any, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenBrace) gaze
        let! statements = (optional (repeatSep patternStatementNib Token.Comma)) gaze
        let! _ = Gaze.attempt (take Token.CloseBrace) gaze
        return Any.Network(Set.ofList statements)
    }

let networkExpressionNib (gaze: Gaze.Gaze<Token>) : Result<Network, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenBrace) gaze
        let! statements = (optional (repeatSep patternStatementNib Token.Comma)) gaze
        let! _ = Gaze.attempt (take Token.CloseBrace) gaze
        return (Set.ofList statements)
    }

let symbolNib (gaze: Gaze.Gaze<Token>) : Result<ElementPattern, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Element(value)) -> Ok(ElementPattern.Element(Element value))
    | Ok(Token.StringLiteral(value)) -> Ok(ElementPattern.Element(Element value))
    | _ -> Error(Gaze.GazeError.NoMatch)

let elementPatternNib (gaze: Gaze.Gaze<Token>) : Result<ElementPattern, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Element(value)) -> Ok(ElementPattern.Element(Element value))
    | Ok(Token.StringLiteral(value)) -> Ok(ElementPattern.Element(Element value))
    | Ok(Token.Variable(value)) -> Ok(ElementPattern.Variable(Variable value))
    | _ -> Error(Gaze.GazeError.NoMatch)

let elementLiteralVariableNib (gaze: Gaze.Gaze<Token>) : Result<Any, Gaze.GazeError> =
    match Gaze.next gaze with
    | Ok(Token.Element(value)) -> Ok(Any.Element(Element value))
    | Ok(Token.StringLiteral(value)) -> Ok(Any.Literal value)
    | Ok(Token.Variable(value)) -> Ok(Any.Variable(Variable value))
    | _ -> Error(Gaze.GazeError.NoMatch)

let anyNib: Gaze.Nibbler<Token, Any> =
    takeFirst [ quoteAnyNib; elementLiteralVariableNib; networkNib; commentNib ]

let valuePatternNib (gaze: Gaze.Gaze<Token>) : Result<Value, Gaze.GazeError> =
    match Gaze.next gaze with
    | Ok(Token.Element(value)) -> Ok(Value.Element(Element value))
    | Ok(Token.StringLiteral(value)) -> Ok(Value.Literal value)
    | Ok(Token.Variable(value)) -> Ok(Value.Variable(Variable value))
    | Ok(Token.OpenSquare) ->
        match partialQuoteNib gaze with
        | Ok value -> Ok(Value.Quote(value))
        | _ -> Error(Gaze.GazeError.NoMatch)
    | _ -> Error(Gaze.GazeError.NoMatch)

let scriptNib = repeat anyNib

/// <summary></summary>
/// <param name="tokens">The list of Tokens to be parsered.</param>
/// <returns>The AST created from the token list of an Error.</returns>
let parse (tokens: Token list) : Result<Script, LigatureError> =
    let tokens =
        List.filter
            (fun token ->
                match token with
                | Token.WhiteSpace(_)
                | Token.Comment
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
