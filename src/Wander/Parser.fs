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
            | Ok(Token.Symbol(name)) -> Ok(WanderValue.Symbol(Symbol name))
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let readSymbol (gaze: Gaze.Gaze<Token>) : Result<WanderValue, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Symbol(value)) -> Ok(WanderValue.Symbol(Symbol value))
    | _ -> Error(Gaze.GazeError.NoMatch)

let expressionNib (gaze: Gaze.Gaze<Token>) : Result<WanderValue, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenParen) gaze
        let! name = Gaze.attempt symbolNib gaze
        let! values = Gaze.attempt (optional (repeat valueNib)) gaze
        let! _ = Gaze.attempt (take Token.CloseParen) gaze
        return WanderValue.Call(name, values)
    }

let statementNib (gaze: Gaze.Gaze<Token>) : Result<(Symbol * Symbol * Symbol), Gaze.GazeError> =
    let entity = symbolNib gaze
    let attribute = symbolNib gaze
    let value = symbolNib gaze

    match (entity, attribute, value) with
    | (Ok(e), Ok(a), Ok(v)) -> Ok(e, a, v)
    | _ -> Error(Gaze.NoMatch)

let networkNib (gaze: Gaze.Gaze<Token>) : Result<WanderValue, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenBrace) gaze
        let! statements = (optional (repeatSep statementNib Token.Comma)) gaze
        let! _ = Gaze.attempt (take Token.CloseBrace) gaze
        return WanderValue.Network(expressNetwork statements)
    }

let symbolNib (gaze: Gaze.Gaze<Token>) : Result<Symbol, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Symbol(value)) -> Ok(Symbol value)
    | Ok(Token.StringLiteral(value)) -> Ok(Symbol value)
    | _ -> Error(Gaze.GazeError.NoMatch)

let symbolValueNib (gaze: Gaze.Gaze<Token>) : Result<WanderValue, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Symbol(value)) -> Ok(WanderValue.Symbol(Symbol value))
    | Ok(Token.StringLiteral(value)) -> Ok(WanderValue.Symbol(Symbol value))
    | _ -> Error(Gaze.GazeError.NoMatch)

let valueNib: Gaze.Nibbler<Token, WanderValue> =
    takeFirst [ expressionNib; symbolValueNib; networkNib ]

let callNib (gaze: Gaze.Gaze<Token>) : Result<Call, Gaze.GazeError> =
    result {
        let! name = Gaze.attempt symbolNib gaze
        let! values = Gaze.attempt (optional (repeat valueNib)) gaze
        return Call(name, values)
    }

let elementNib = takeFirst [ expressionNib; networkNib ]

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

/// Helper function that handles tokienization for you.
let parseString (input: string) =
    match tokenize input with
    | Ok tokens -> parse tokens
    | Error err -> error "Could not parse input." None //error $"Could not match from {gaze.offset} - {(Gaze.remaining gaze)}." None //TODO this error message needs updated

let expressNetwork (network: (Symbol * Symbol * Symbol) list) : Set<Entry> =
    let res: Set<Entry> = (List.map (elementTupleToEntry) network) |> Set.ofSeq
    res

let elementTupleToEntry ((entity, attribute, value): (Symbol * Symbol * Symbol)) : Entry =

    if attribute = Symbol ":" then
        Entry.Extends { element = entity; concept = value }
    else if attribute = Symbol ":¬" then
        Entry.NotExtends { element = entity; concept = value }
    else
        Entry.Role
            { first = entity
              second = value
              role = attribute }
