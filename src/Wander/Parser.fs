// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module rec Wander.Parser

open Tokenizer
open FsToolkit.ErrorHandling
open Nibblers
open Ligature.Model
open Model

let identifierNib (gaze: Gaze.Gaze<Token>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.Element(name)) -> Ok(Value.Element(Element name))
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let elementNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    match Gaze.next gaze with
    | Error(err) -> Error err
    | Ok(Token.Element(value)) -> Ok(Element value)
    | _ -> Error(Gaze.GazeError.NoMatch)

let equalNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    match Gaze.next gaze with
    | Error(err) -> Error err
    | Ok(Token.Element("=")) -> Ok(Element "=")
    | _ -> Error(Gaze.GazeError.NoMatch)

let variableNib (gaze: Gaze.Gaze<Token>) : Result<Variable, Gaze.GazeError> =
    match Gaze.next gaze with
    | Error(err) -> Error err
    | Ok(Token.Variable(value)) -> Ok(Variable value)
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

let patternStatementNib
    (gaze: Gaze.Gaze<Token>)
    : Result<(ElementPattern * ElementPattern * ValuePattern), Gaze.GazeError> =
    let entity = elementPatternNib gaze
    let attribute = elementPatternNib gaze
    let value = Gaze.attempt valuePatternNib gaze

    match (entity, attribute, value) with
    | (Ok(e), Ok(a), Ok(v)) -> Ok(e, a, v)
    | _ -> Error(Gaze.NoMatch)

let patternNib (gaze: Gaze.Gaze<Token>) : Result<Any, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenBrace) gaze
        let! statements = (optional (repeatSep patternStatementNib Token.Comma)) gaze
        let! _ = Gaze.attempt (take Token.CloseBrace) gaze
        return Any.Pattern(expressPattern statements)
    }

let symbolNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Element(value)) -> Ok(Element value)
    | Ok(Token.StringLiteral(value)) -> Ok(Element value)
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
    takeFirst [ quoteNib; elementLiteralVariableNib; networkNib; patternNib ]

let valueNib (gaze: Gaze.Gaze<Token>) : Result<Value, Gaze.GazeError> =
    match Gaze.next gaze with
    | Ok(Token.Element(value)) -> Ok(Value.Element(Element value))
    | Ok(Token.StringLiteral(value)) -> Ok(Value.Literal value)
    | _ -> Error(Gaze.GazeError.NoMatch)

let valuePatternNib (gaze: Gaze.Gaze<Token>) : Result<ValuePattern, Gaze.GazeError> =
    match Gaze.next gaze with
    | Ok(Token.Element(value)) -> Ok(ValuePattern.Element(Element value))
    | Ok(Token.StringLiteral(value)) -> Ok(ValuePattern.Literal value)
    | Ok(Token.Variable(value)) -> Ok(ValuePattern.Variable(Variable value))
    | _ -> Error(Gaze.GazeError.NoMatch)

let callNib (gaze: Gaze.Gaze<Token>) : Result<Call, Gaze.GazeError> =
    result {
        let! name = Gaze.attempt elementNib gaze
        let! arguments = Gaze.attempt (optional (repeat anyNib)) gaze
        return name, arguments
    }

let callExpressionNib (gaze: Gaze.Gaze<Token>) : Result<Expression, Gaze.GazeError> =
    result {
        let! name = Gaze.attempt elementNib gaze
        let! arguments = Gaze.attempt (optional (repeat anyNib)) gaze
        return Expression.Call(name, arguments)
    }

let anyAssignmentNib (gaze: Gaze.Gaze<Token>) : Result<Expression, Gaze.GazeError> =
    result {
        let! variable = Gaze.attempt variableNib gaze
        let! _ = Gaze.attempt equalNib gaze
        let! value = Gaze.attempt anyNib gaze
        return Expression.AnyAssignment(variable, value)
    }

let callAssignmentNib (gaze: Gaze.Gaze<Token>) : Result<Expression, Gaze.GazeError> =
    result {
        let! variable = Gaze.attempt variableNib gaze
        let! _ = Gaze.attempt equalNib gaze
        let! call = Gaze.attempt callNib gaze
        return Expression.CallAssignment(variable, call)
    }

let scriptNib =
    repeatSep (takeFirst [ callExpressionNib; callAssignmentNib; anyAssignmentNib ]) Token.Comma

/// <summary></summary>
/// <param name="tokens">The list of Tokens to be parsered.</param>
/// <returns>The AST created from the token list of an Error.</returns>
let parse (tokens: Token list) : Result<Script, LigatureError> =
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

let expressPattern (network: (ElementPattern * ElementPattern * ValuePattern) list) : Set<EntryPattern> =
    let res: Set<EntryPattern> =
        (List.map (elementTupleToEntryPattern) network) |> Set.ofSeq

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

let elementTupleToEntryPattern (tuple: (ElementPattern * ElementPattern * ValuePattern)) : EntryPattern =
    match tuple with
    | (element, attribute, value) ->
        { element = element
          attribute = attribute
          value = value }
