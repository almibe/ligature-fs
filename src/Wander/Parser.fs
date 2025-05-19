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
    | Ok(Token.Literal value) ->
        Ok(
            Value.Literal
                { content = value
                  datatype = Some(Term "")
                  langTag = None }
        )
    | Error err -> Error err
    | _ -> Error Gaze.GazeError.NoMatch

let variableNib (gaze: Gaze.Gaze<Token>) : Result<Variable, Gaze.GazeError> =
    match Gaze.next gaze with
    | Error err -> Error err
    | Ok(Token.Variable value) -> Ok(Variable value)
    | _ -> Error Gaze.GazeError.NoMatch

let slotNib (gaze: Gaze.Gaze<Token>) : Result<Slot, Gaze.GazeError> =
    match Gaze.next gaze with
    | Error err -> Error err
    | Ok(Token.Slot value) -> Ok(Slot value)
    | _ -> Error Gaze.GazeError.NoMatch

let partialTupleNib (gaze: Gaze.Gaze<Token>) : Result<Tuple, Gaze.GazeError> =
    result {
        let! values = Gaze.attempt (optional (repeat anyNib)) gaze
        let! _ = Gaze.attempt (take Token.CloseSquare) gaze
        return values
    }

let applicationNib (gaze: Gaze.Gaze<Token>) : Result<Any, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenParen) gaze
        let! fn = Gaze.attempt termNib gaze
        let! values = Gaze.attempt scriptNib gaze
        let! _ = Gaze.attempt (take Token.CloseParen) gaze
        return Any.NodeExpression { name = fn; attributes = Map.empty; children = values}
    }

let tupleAnyNib (gaze: Gaze.Gaze<Token>) : Result<Any, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenSquare) gaze
        let! values = Gaze.attempt (optional (repeat anyNib)) gaze
        let! _ = Gaze.attempt (take Token.CloseSquare) gaze
        return Any.Tuple values
    }

let argsNib (gaze: Gaze.Gaze<Token>) : Result<Variable list, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenSquare) gaze
        let! values = Gaze.attempt (optional (repeat variableNib)) gaze
        let! _ = Gaze.attempt (take Token.CloseSquare) gaze
        return values
    }

let nodeNib (gaze: Gaze.Gaze<Token>) : Result<Any, Gaze.GazeError> =
    let node =
        result {
            let! _ = Gaze.attempt (take Token.OpenBrace) gaze
            let! name = Gaze.attempt termNib gaze
            let! attributes = Gaze.attempt attributesNib gaze
            let! children = Gaze.attempt (optional (repeat anyNib)) gaze
            let! _ = Gaze.attempt (take Token.CloseBrace) gaze
            return name, attributes, children
        }

    match node with
    | Ok (name, attributes, children) ->
        failwith "TODO"
        //Ok Any.NodeLiteral ()
        // if res.Length % 2 = 0 then
        //     let res =
        //         List.fold
        //             (fun state value ->
        //                 match value with
        //                 | [ first; second ] -> Map.add first second state
        //                 | _ -> failwith "TODO")
        //             Map.empty
        //             (List.chunkBySize 2 res)

        //     Ok(Any.Node res)
        // else
        //     Error Gaze.NoMatch
    | Error err -> Error err

let symbolNib (gaze: Gaze.Gaze<Token>) : Result<TermPattern, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error err -> Error err
    | Ok(Token.Term value) -> Ok(TermPattern.Term(Term value))
    | Ok(Token.Literal value) -> Ok(TermPattern.Term(Term value))
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
    | Ok(Token.Literal value) ->
        Ok(
            ValuePattern.Literal
                { content = value
                  datatype = None
                  langTag = None }
        )
    | Ok(Token.Slot value) -> Ok(ValuePattern.Slot(Slot value))
    | _ -> Error Gaze.GazeError.NoMatch

let elementLiteralSlotNib (gaze: Gaze.Gaze<Token>) : Result<Any, Gaze.GazeError> =
    match Gaze.next gaze with
    | Ok(Token.Term value) -> Ok(Any.Term(Term value))
    | Ok(Token.Slot value) -> Ok(Any.Slot(Slot value))
    | Ok(Token.Literal value) ->
        Ok(
            Any.Literal
                { content = value
                  datatype = None
                  langTag = None }
        )
    | Ok(Token.Variable variable) -> Ok(Any.Variable(Variable variable))
    | _ -> Error Gaze.GazeError.NoMatch

let attributesNib (gaze: Gaze.Gaze<Token>): Result<(Term * Any) list, Gaze.GazeError> =
    let mutable res = []
    let mutable cont = true
    while cont do
        match Gaze.peek gaze with
        | Ok (Token.Term value) -> 
            
            failwith "TODO"
        | _ -> failwith "TODO"
    Ok res
//    takeFirst [ applicationNib; tupleAnyNib; nodeNib; elementLiteralSlotNib ]

let anyNib: Gaze.Nibbler<Token, Any> =
    takeFirst [ applicationNib; tupleAnyNib; nodeNib; elementLiteralSlotNib ]

let expressionNib (gaze: Gaze.Gaze<Token>) : Result<Any, Gaze.GazeError> =
    match anyNib gaze with
    | Ok res -> Ok res
    | _ -> Error Gaze.GazeError.NoMatch

let scriptNib: Gaze.Nibbler<Token, Script> = repeatOptional anyNib

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
