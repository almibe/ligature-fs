// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module rec Wander.Parser

open Tokenizer
open FsToolkit.ErrorHandling
open Nibblers
open Ligature.Model
open Model

let termNib (gaze: Gaze.Gaze<Token>) : Result<Term, Gaze.GazeError> =
    match Gaze.next gaze with
    | Ok(Token.Term value) -> Ok(Term value)
    | Error err -> Error err
    | _ -> Error Gaze.GazeError.NoMatch

let individualNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> = failwith "TODO"
// match Gaze.next gaze with
// | Ok(Token.Term value) -> Ok(Value.Term(Term value))
// | Ok(Token.Literal value) ->
//     Ok(
//         Value.Literal
//             { id = value
//               datatype = Some(Term "")
//               langTag = None }
//     )
// | Error err -> Error err
// | _ -> Error Gaze.GazeError.NoMatch

let variableNib (gaze: Gaze.Gaze<Token>) : Result<Variable, Gaze.GazeError> =
    match Gaze.next gaze with
    | Error err -> Error err
    | Ok(Token.Variable value) -> Ok(Variable value)
    | _ -> Error Gaze.GazeError.NoMatch

let argsNib (gaze: Gaze.Gaze<Token>) : Result<Variable list, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenSquare) gaze
        let! values = Gaze.attempt (optional (repeat variableNib)) gaze
        let! _ = Gaze.attempt (take Token.CloseSquare) gaze
        return values
    }

let variableApplicationNib (gaze: Gaze.Gaze<Token>) : Result<Expression, Gaze.GazeError> =
    let node =
        result {
            let! name = Gaze.attempt variableNib gaze
            let! _ = Gaze.attempt (take Token.OpenParen) gaze
            let! attributes = Gaze.attempt attributesNib gaze
            let! children = Gaze.attempt (repeatOptional anyNib) gaze
            let! _ = Gaze.attempt (take Token.CloseParen) gaze
            return name, attributes, children
        }

    match node with
    | Ok(name, attributes, children) ->
        Ok(
            Expression.VariableApplication
                { variable = name
                  attributes = attributes
                  children = children }
        )
    | Error err -> Error err


let applicationNib (gaze: Gaze.Gaze<Token>) : Result<Expression, Gaze.GazeError> =
    let node =
        result {
            let! name = Gaze.attempt termNib gaze
            let! _ = Gaze.attempt (take Token.OpenParen) gaze
            let! attributes = Gaze.attempt attributesNib gaze
            let! children = Gaze.attempt (repeatOptional anyNib) gaze
            let! _ = Gaze.attempt (take Token.CloseParen) gaze
            return name, attributes, children
        }

    match node with
    | Ok(name, attributes, children) ->
        Ok(
            Expression.Application
                { name = name
                  attributes = attributes
                  arguments = children }
        )
    | Error err -> Error err

let objectNib (gaze: Gaze.Gaze<Token>) : Result<Expression, Gaze.GazeError> =
    let node =
        result {
            let! name = Gaze.attempt termNib gaze
            let! _ = Gaze.attempt (take Token.OpenBrace) gaze
            let! attributes = Gaze.attempt attributesNib gaze
            let! children = Gaze.attempt (repeatOptional anyNib) gaze
            let! _ = Gaze.attempt (take Token.CloseBrace) gaze
            return name, attributes, children
        }

    match node with
    | Ok(Term name, attributes, children) ->
        Ok(
            Expression.ObjectView
                { root =
                    { value = name
                      space = None
                      langTag = None }
                  concepts = Set.empty
                  links = Map.empty }
        // { name = name
        //   attributes = attributes
        //   children = children }
        )
    | Error err -> Error err

let elementLiteralSlotNib (gaze: Gaze.Gaze<Token>) : Result<Expression, Gaze.GazeError> =
    match Gaze.next gaze with
    | Ok(Token.Term value) -> Ok(Expression.Term(Term value))
    // | Ok(Token.Slot value) -> Ok(Expression.Slot(Slot value))
    | Ok(Token.Literal value) ->
        Ok(
            Expression.Element
                { value = value
                  space = None
                  langTag = None }
        )
    | Ok(Token.Variable variable) -> Ok(Expression.Variable(Variable variable))
    | _ -> Error Gaze.GazeError.NoMatch

let attributesNib (gaze: Gaze.Gaze<Token>) : Result<Map<Term, Expression>, Gaze.GazeError> =
    let mutable res = Map.empty
    let mutable cont = true

    while cont do
        match Gaze.attempt (takeAll [ termAnyNib; equalAnyNib; anyNib ]) gaze with
        | Ok [ Expression.Term name; Expression.Term(Term "="); value ] -> res <- Map.add name value res
        | _ -> cont <- false

    Ok res

let equalAnyNib gaze : Result<Expression, Gaze.GazeError> =
    match Gaze.attempt termNib gaze with
    | Ok(Term "=") -> Ok(Expression.Term(Term "="))
    | _ -> Error Gaze.NoMatch

let termAnyNib: Gaze.Nibbler<Token, Expression> =
    takeFirst [ elementLiteralSlotNib ]

let anyNib: Gaze.Nibbler<Token, Expression> =
    takeFirst [ variableApplicationNib; applicationNib; objectNib; elementLiteralSlotNib ]

let lineNib (gaze: Gaze.Gaze<Token>) : Result<Variable option * Expression, Gaze.GazeError> =
    match Gaze.attempt variableNib gaze with
    | Ok variable ->
        match Gaze.attempt termNib gaze with
        | Ok(Term "=") ->
            match Gaze.attempt anyNib gaze with
            | Ok res -> Ok(Some variable, res)
            | _ -> Error Gaze.NoMatch
        | Ok _ -> Error Gaze.NoMatch
        | Error Gaze.NoMatch -> Ok(None, Expression.Variable variable)
    | Error Gaze.NoMatch ->
        match Gaze.attempt anyNib gaze with
        | Ok res -> Ok(None, res)
        | _ -> Error Gaze.NoMatch

/// <summary></summary>
/// <returns></returns>
let expressionsNib: Gaze.Nibbler<Token, Expression list> = repeatOptional anyNib


let scriptNib: Gaze.Nibbler<Token, Script> = repeatOptional (takeFirst [ lineNib ])

/// <summary></summary>
/// <param name="tokens">The list of Tokens to be parsered.</param>
/// <returns>The AST created from the token list of an Error.</returns>
let parse (tokens: Token list) : Result<Script, LigatureError> =
    let tokens =
        List.filter
            (fun token ->
                match token with
                | Token.WhiteSpace _
                | Token.Comment
                | Token.NewLine _ -> false
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

/// <summary></summary>
/// <param name="tokens">The list of Tokens to be parsered.</param>
/// <returns>The AST created from the token list of an Error.</returns>
let read (tokens: Token list) : Result<Expression, LigatureError> =
    let tokens =
        List.filter
            (fun token ->
                match token with
                | Token.WhiteSpace _
                | Token.NewLine _ -> false
                | _ -> true)
            tokens

    if tokens.IsEmpty then
        error "Illegal call to read." None
    else
        let gaze = Gaze.fromList tokens

        match Gaze.attempt anyNib gaze with
        | Ok res ->
            if Gaze.isComplete gaze then
                Ok res
            else
                error $"Failed to read completely. {Gaze.remaining gaze}" None
        | Error err -> error $"Failed to parse.\n{err.ToString()}" None

/// Helper function that handles tokienization for you.
let parseString (input: string) =
    match tokenize input with
    | Ok tokens -> parse tokens
    | Error _ -> error "Could not parse input." None //error $"Could not match from {gaze.offset} - {(Gaze.remaining gaze)}." None //TODO this error message needs updated
