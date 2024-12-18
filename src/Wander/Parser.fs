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

let patternStatementNib (gaze: Gaze.Gaze<Token>) : Result<(ElementPattern * ElementPattern * Value), Gaze.GazeError> =
    let entity = elementPatternNib gaze
    let attribute = elementPatternNib gaze
    let value = Gaze.attempt valuePatternNib gaze

    match (entity, attribute, value) with
    | (Ok(e), Ok(a), Ok(v)) -> Ok(e, a, v)
    | _ -> Error(Gaze.NoMatch)

let networkNib (gaze: Gaze.Gaze<Token>) : Result<Any, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenBrace) gaze
        let! statements = (optional (repeatSep patternStatementNib Token.Comma)) gaze
        let! _ = Gaze.attempt (take Token.CloseBrace) gaze
        return Any.Network(Set.ofList statements)
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

let pipeNib (gaze: Gaze.Gaze<Token>) : Result<Any, Gaze.GazeError> =
    match Gaze.next gaze with
    | Ok(Token.Pipe) -> Ok(Any.Pipe)
    | _ -> Error(Gaze.GazeError.NoMatch)

let anyNib: Gaze.Nibbler<Token, Any> =
    takeFirst [ quoteNib; elementLiteralVariableNib; networkNib; networkNib; pipeNib ]

let anyNibIgnorePipe: Gaze.Nibbler<Token, Any> =
    takeFirst [ quoteNib; elementLiteralVariableNib; networkNib; networkNib ]

let valueNib (gaze: Gaze.Gaze<Token>) : Result<Value, Gaze.GazeError> =
    match Gaze.next gaze with
    | Ok(Token.Element(value)) -> Ok(Value.Element(Element value))
    | Ok(Token.StringLiteral(value)) -> Ok(Value.Literal value)
    | _ -> Error(Gaze.GazeError.NoMatch)

let valuePatternNib (gaze: Gaze.Gaze<Token>) : Result<Value, Gaze.GazeError> =
    match Gaze.next gaze with
    | Ok(Token.Element(value)) -> Ok(Value.Element(Element value))
    | Ok(Token.StringLiteral(value)) -> Ok(Value.Literal value)
    | Ok(Token.Variable(value)) -> Ok(Value.Variable(Variable value))
    | _ -> Error(Gaze.GazeError.NoMatch)

let callToQuote ((name, args): Call) : Quote = List.append [ Any.Element name ] args

let callNib (gaze: Gaze.Gaze<Token>) : Result<Call, Gaze.GazeError> =
    let mutable cont = true
    let mutable currentCommandName: Element = Element ""
    let mutable call = None

    while cont do
        match Gaze.peek gaze with
        | Ok Token.Comma -> cont <- false
        | Ok Token.Pipe ->
            if call = None then
                cont <- false
            else
                Gaze.next gaze |> ignore

                match Gaze.next gaze with
                | Ok(Token.Element name) -> currentCommandName <- Element name
                | _ -> failwith "TODO"
        | Error _ -> cont <- false
        | Ok(Token.Element e) ->
            currentCommandName <- Element e
            Gaze.next gaze |> ignore
        | Ok _ ->
            call <- None
            cont <- false

        if cont then
            let arguments = Gaze.attempt (optional (repeat anyNibIgnorePipe)) gaze

            match arguments with
            | Ok arguments ->
                match call with
                | None -> call <- Some(Call(currentCommandName, arguments))
                | Some prevCall ->
                    let quote = callToQuote prevCall
                    let newArgs = List.append arguments [ Any.Quote quote ]
                    call <- Some(Call(currentCommandName, newArgs))
            | Error _ -> failwith "TODO"

    match call with
    | Some call -> Ok call
    | _ -> Error Gaze.NoMatch

let callExpressionNib (gaze: Gaze.Gaze<Token>) : Result<Expression, Gaze.GazeError> =
    result {
        let! call = Gaze.attempt callNib gaze
        return Expression.Call(call)
    }

let anyAssignmentNib (gaze: Gaze.Gaze<Token>) : Result<Expression, Gaze.GazeError> =
    result {
        let! variable = Gaze.attempt variableNib gaze
        let! _ = Gaze.attempt equalNib gaze
        let! value = Gaze.attempt anyNibIgnorePipe gaze
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

        match Gaze.attempt anyNibIgnorePipe gaze with
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
