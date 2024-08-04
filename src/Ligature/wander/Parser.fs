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
    | Word of string
    | NetworkName of string
    | Pipeline of Element list
    | String of string
    | Int of bigint
    | Bytes of byte array
    | Slot of Slot
    | Call of string * Element list
    | Network of (Element * Element * Element) list

let nameStrNibbler (gaze: Gaze.Gaze<Token>) : Result<string, Gaze.GazeError> =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.Word(value)) -> Ok(value)
            | _ -> Error Gaze.GazeError.NoMatch)
        gaze

let wordNib (gaze: Gaze.Gaze<Token>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.Word(name)) -> Ok(Element.Word(name))
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
        let! word = Gaze.attempt (wordNib) gaze
        let! values = Gaze.attempt (optional (repeatN pipelineNib 1)) gaze

        match (word, values) with
        | (Element.Word(word), []) -> return Element.Call(word, values)
        | (Element.Word(word), [ Element.Pipeline(quote) ]) -> return Element.Call(word, quote)
        | (_, _) -> return failwith "TODO"
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

let statementNib (gaze: Gaze.Gaze<Token>) : Result<(Element * Element * Element), Gaze.GazeError> =
    let entity = patternNib gaze
    let attribute = patternNib gaze

    let value =
        match Gaze.check valueNib gaze with
        | Ok(_) -> valueNib gaze
        | Error(_) -> quotekNib gaze

    match (entity, attribute, value) with
    | (Ok(e), Ok(a), Ok(v)) -> Ok(e, a, v)
    | _ -> Error(Gaze.NoMatch)

let quotekNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenSquare) gaze
        let! values = (optional (repeat elementNib)) gaze
        let! _ = Gaze.attempt (take Token.CloseSquare) gaze
        return Element.Pipeline(values)
    }

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
    | Ok(Token.Word(value)) -> Ok(Element.Word value)
    | Ok(Token.Slot(value)) -> Ok(Element.Slot(value))
    | _ -> Error(Gaze.GazeError.NoMatch)

/// Read the next Element from the given instance of Gaze<Token>
let valueNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Int(value)) -> Ok(Element.Int value)
    | Ok(Token.Word(value)) -> Ok(Element.Word value)
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
            | Ok(Token.Word w) -> List.append elements [ (Element.Word w) ]
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

let readWord (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Word(value)) -> Ok(Element.Word value)
    | _ -> Error(Gaze.GazeError.NoMatch)

let readSlot (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Slot(value)) -> Ok(Element.Slot value)
    | _ -> Error(Gaze.GazeError.NoMatch)

//let patternMatchBodyNib = takeFirst [ networkNib; wordNib; pipelineNib ]

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
    | Element.Pipeline(q) -> handlePipeline q
    | Element.Slot s -> LigatureValue.Slot s
    | Element.String s -> LigatureValue.String s
    | Element.Word w -> LigatureValue.Word(Word w)
    | Element.Call(w, []) -> LigatureValue.Word(Word w)

let handlePipeline (quote: Element list) : LigatureValue =
    let res = List.map (fun element -> elementToValue element) quote

    LigatureValue.Pipeline(res) //({ parameters = []; value = res })

let elementTupleToStatement ((e, a, v): (Element * Element * Element)) : (PatternWord * PatternWord * LigatureValue) =
    let entity =
        match e with
        | Element.Word w -> PatternWord.Word(Word w)
        | Element.Slot s -> PatternWord.Slot s
        | _ -> failwith "TODO"

    let attribute =
        match a with
        | Element.Word w -> PatternWord.Word(Word w)
        | Element.Slot s -> PatternWord.Slot s
        | _ -> failwith "TODO"

    let value =
        match v with
        | Element.Word w -> LigatureValue.Word(Word w)
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
        // | Element.Word w ->
        //     express tail (List.append expressions [ Expression.Call(Word(w), { parameterNames = []; quote = [] }) ])
        | Element.Call(w, q) ->
            //            List.map (fun x -> express x) q
            express tail (List.append expressions [ Expression.Call(Word(w)) ])
        | Element.NetworkName name -> express tail (List.append expressions [ Expression.NetworkName name ])
        | _ -> failwith "TODO"
