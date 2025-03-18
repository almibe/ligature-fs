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

let valueNib (gaze: Gaze.Gaze<Token>) : Result<Value, Gaze.GazeError> =
    match Gaze.next gaze with
    | Ok(Token.Term value) -> Ok(Value.Term(Term value))
    | Ok(Token.StringLiteral value) -> Ok(Value.Literal(Literal value))
    | Error err -> Error err
    | _ -> Error Gaze.GazeError.NoMatch

let variableNib (gaze: Gaze.Gaze<Token>) : Result<Slot, Gaze.GazeError> =
    match Gaze.next gaze with
    | Error err -> Error err
    | Ok(Token.Slot value) -> Ok(Slot value)
    | _ -> Error Gaze.GazeError.NoMatch

let partialQuoteNib (gaze: Gaze.Gaze<Token>) : Result<Quote, Gaze.GazeError> =
    result {
        let! values = Gaze.attempt (optional (repeat anyNib)) gaze
        let! _ = Gaze.attempt (take Token.CloseSquare) gaze
        return values
    }

let blockNib (gaze: Gaze.Gaze<Token>) : Result<Any, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenParen) gaze
        let! values = Gaze.attempt scriptNib gaze
        let! _ = Gaze.attempt (take Token.CloseParen) gaze
        return Any.Block(values)
    }

let quoteAnyNib (gaze: Gaze.Gaze<Token>) : Result<Any, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenSquare) gaze
        let! values = Gaze.attempt (optional (repeat anyNib)) gaze
        let! _ = Gaze.attempt (take Token.CloseSquare) gaze
        return Any.Quote values
    }

let recordNib (gaze: Gaze.Gaze<Token>) : Result<Any, Gaze.GazeError> =
    let body =
        result {
            let! _ = Gaze.attempt (take Token.OpenBrace) gaze
            let! values = Gaze.attempt (optional (repeat anyNib)) gaze
            let! _ = Gaze.attempt (take Token.CloseBrace) gaze
            return values
        }

    match body with
    | Ok res ->
        if res.Length % 2 = 0 then
            let res =
                List.fold
                    (fun state value ->
                        match value with
                        | [ first; second ] -> Map.add first second state
                        | _ -> failwith "TODO")
                    Map.empty
                    (List.chunkBySize 2 res)

            Ok(Any.Record res)
        else
            Error Gaze.NoMatch
    | Error err -> Error err

let pipeNib (gaze: Gaze.Gaze<Token>) : Result<Any, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.Pipe) gaze
        return Any.Pipe
    }

let symbolNib (gaze: Gaze.Gaze<Token>) : Result<TermPattern, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error err -> Error err
    | Ok(Token.Term value) -> Ok(TermPattern.Term(Term value))
    | Ok(Token.StringLiteral value) -> Ok(TermPattern.Term(Term value))
    | _ -> Error Gaze.GazeError.NoMatch

let elementPatternNib (gaze: Gaze.Gaze<Token>) : Result<TermPattern, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error err -> Error err
    | Ok(Token.Term value) -> Ok(TermPattern.Term(Term value))
    | Ok(Token.Slot value) -> Ok(TermPattern.Slot(Slot value))
    | _ -> Error Gaze.GazeError.NoMatch

let valuePatternNib (gaze: Gaze.Gaze<Token>) : Result<ValuePattern, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error err -> Error err
    | Ok(Token.Term value) -> Ok(ValuePattern.Term(Term value))
    | Ok(Token.StringLiteral value) -> Ok(ValuePattern.Literal(Literal value))
    | Ok(Token.Slot value) -> Ok(ValuePattern.Slot(Slot value))
    | _ -> Error Gaze.GazeError.NoMatch

let elementLiteralSlotNib (gaze: Gaze.Gaze<Token>) : Result<Any, Gaze.GazeError> =
    match Gaze.next gaze with
    | Ok(Token.Term value) -> Ok(Any.Term(Term value))
    | Ok(Token.Slot value) -> Ok(Any.Slot(Slot value))
    | Ok(Token.StringLiteral value) -> Ok(Any.Literal(Literal value))
    | _ -> Error Gaze.GazeError.NoMatch

let anyNib: Gaze.Nibbler<Token, Any> =
    takeFirst [ quoteAnyNib; recordNib; elementLiteralSlotNib; blockNib; pipeNib ]

let assignmentNib (gaze: Gaze.Gaze<Token>) : Result<Expression, Gaze.GazeError> =
    let letKeyword = Gaze.attempt anyNib gaze
    if letKeyword = Ok (Any.Term(Term "let")) then        
        match Gaze.attempt anyNib gaze with
        | Ok (Any.Term name) ->
            let res =
                takeWhile (fun value ->
                    match value with
                    | Token.Term "=" -> false
                    | Token.Term _ -> true
                    | _ -> false) gaze
            match res with
            | Ok args -> 
                match Gaze.next gaze with
                | Ok(Token.Term "=") -> 
                    match Gaze.attempt anyNib gaze with
                    | Ok value -> 
                        Ok (Expression.Assignment (name, [], value))
                    | _ -> Error Gaze.GazeError.NoMatch
                | _ -> Error Gaze.GazeError.NoMatch
            | _ -> Error Gaze.GazeError.NoMatch
            // let assignmentOp = Gaze.attempt anyNib gaze
            // let value = Gaze.attempt anyNib gaze

            // match letKeyword, variable, assignmentOp, value with
            // | Ok(Any.Term(Term "let")), Ok(Any.Term _), Ok(Any.Term(Term "=")), Ok value ->
            //     //Ok (Expression.Assignment )
            //     failwith "TODO"
            // | _ -> 
        | _ -> Error Gaze.GazeError.NoMatch
    else
        Error Gaze.GazeError.NoMatch


let applicationNib (gaze: Gaze.Gaze<Token>) : Result<Expression, Gaze.GazeError> =
    match repeat anyNib gaze with
    | Ok res -> Ok(Expression.Application res)
    | _ -> Error Gaze.GazeError.NoMatch

let scriptNib =
    optional (repeatSep (takeFirst [ assignmentNib; applicationNib ]) Token.Comma)

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
