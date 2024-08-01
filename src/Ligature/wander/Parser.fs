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
    | Quote of string list * Element list
    | String of string
    | Int of bigint
    | Bytes of byte array
    | Slot of Slot
    | Call of string * Element list
    | Network of (Element * Element * Element) list

// and EntityDescription = Identifier * Element list

// and NetworkRoot = Identifier * EntityDescription list

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

let callNib (gaze: Gaze.Gaze<Token>) =
    result {
        let! word = Gaze.attempt (wordNib) gaze
        let! values = Gaze.attempt (optional (repeatN quoteNib 1)) gaze

        match (word, values) with
        | (Element.Word(word), []) -> return Element.Call(word, values)
        | (Element.Word(word), [ Element.Quote(name, quote) ]) -> return Element.Call(word, quote)
        | (_, _) -> return failwith "TODO"
    }

let readInteger (gaze: Gaze.Gaze<Token>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.Int(i)) -> Ok(Element.Int(i))
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let wideArrowNib (gaze: Gaze.Gaze<Token>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.Arrow) -> Ok(())
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let quoteNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenSquare) gaze
        let! values = Gaze.attempt (optional (repeat elementNib)) gaze
        let! _ = Gaze.attempt (take Token.CloseSquare) gaze
        return Element.Quote([], values)
    }

// let rec readEntityDescription gaze : Result<EntityDescription, Gaze.GazeError> =
//     let res = result {
//         let! attribute = Gaze.attempt (takeFirst [ readIdentifier ]) gaze //TODO only match Identifier or Name
//         let! values = Gaze.attempt readLigatureValues gaze //TODO match single LigatureValue or List of LigatureValues
//         return (attribute, values)
//     }
//     match res with
//     | Ok((Element.Identifier(identifier), res)) -> Ok(identifier, res)
//     | _ -> failwith "TODO"

// let singleEntityDescriptNib gaze =
//     let res = result {
//         let! entity = Gaze.attempt (takeFirst [ readIdentifier; readSlot; wordNib ]) gaze //TODO only match Identifier or Name
//         let! entityDescriptions = readEntityDescription gaze //Gaze.attempt (repeat readEntityDescription) gaze
//         return (entity, [ entityDescriptions ])
//     }
//     match res with
//     | Ok(Element.Identifier(entity), descriptions) -> Ok(NetworkRoot(entity, descriptions))
//     | _ -> failwith "TODO"

// let networkRootNib (gaze: Gaze.Gaze<Token>) : Result<NetworkRoot, Gaze.GazeError> =
//     let singleEntityDescription = Gaze.attempt singleEntityDescriptNib gaze

//     match singleEntityDescription with
//     | Ok _ -> singleEntityDescription
//     | _ ->
//         result {
//             let! entity = Gaze.attempt (takeFirst [ readIdentifier ]) gaze //TODO only match Identifier or Name
//             let! _ = Gaze.attempt (take Token.OpenBrace) gaze
//             let! entityDescriptions = (repeatSep readEntityDescription Token.Comma) gaze //Gaze.attempt (repeat readEntityDescription) gaze
//             let! _ = Gaze.attempt (take Token.CloseBrace) gaze
//             return (failwith "TODO")
//         // return NetworkRoot(entity, entityDescriptions)
//         }

let statementNib (gaze: Gaze.Gaze<Token>) : Result<(Element * Element * Element), Gaze.GazeError> =
    let entity = wordNib gaze
    let attribute = wordNib gaze

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
        let! first = (optional (repeat elementNib)) gaze
        let! second = Gaze.attempt (optional (repeatN (take Token.Arrow) 1)) gaze
        let! third = (optional (repeat elementNib)) gaze
        let! _ = Gaze.attempt (take Token.CloseSquare) gaze

        let (names, contents) =
            match (first, second, third) with
            | [], [], [] -> ([], [])
            | parts, [], [] -> ([], parts)
            | names, [ _ ], parts ->
                let names =
                    List.map
                        (fun name ->
                            match name with
                            | Element.Word(name) -> name
                            | Element.Call(name, []) -> name
                            | _ -> failwith "Error")
                        names

                (names, parts)
            | _ -> failwith ""

        return Element.Quote(names, contents)
    }

let networkNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenBrace) gaze
        let! statements = (optional (repeatSep statementNib Token.Comma)) gaze
        let! _ = Gaze.attempt (take Token.CloseBrace) gaze
        return Element.Network(statements)
    }

/// Read the next Element from the given instance of Gaze<Token>
let valueNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Int(value)) -> Ok(Element.Int value)
    | Ok(Token.Word(value)) -> Ok(Element.Word value)
    | Ok(Token.Slot(value)) -> Ok(Element.Slot(value))
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

/// Read the Value position of the
// let readValues (gaze: Gaze.Gaze<Token>) : Result<Element list, Gaze.GazeError> =
//     let next = Gaze.next gaze

//     match next with
//     | Error(err) -> Error err
//     | Ok(Token.Bytes(value)) -> Ok([ Element.Bytes(value) ])
//     | Ok(Token.Int(value)) -> Ok([ Element.Int value ])
//     | Ok(Token.Word(value)) -> Ok([ Element.Word value ])
//     | Ok(Token.StringLiteral(value)) -> Ok([ Element.String value ])
//     | Ok(Token.Slot(slot)) -> Ok([ Element.Slot slot ])
//     | Ok(Token.OpenSquare) -> readValueList [] gaze
//     | _ ->
//         match Gaze.attempt wordNib gaze with
//         | Ok res -> Ok([ res ])
//         | Error err -> Error(Gaze.GazeError.NoMatch)

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

//let patternMatchBodyNib = takeFirst [ networkNib; wordNib; quoteNib ]

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

let expressQuote values = failwith "TODO"
// let res = List.map (fun value -> expressElement value) values
// Expression.Quote res

// let expressEntityDescription (entityDescription: EntityDescription) =
//     let (attribute, values) = entityDescription
//     (attribute, (List.map (fun value -> expressElement value) values))

let elementToValue (element: Element) : LigatureValue =
    match element with
    | Element.Int i -> LigatureValue.Int i
    | Element.Bytes b -> LigatureValue.Bytes b
    | Element.Network n -> LigatureValue.Network(handleNetwork n)
    | Element.Quote(p, q) -> handleQuote p q
    | Element.Slot s -> LigatureValue.Slot s
    | Element.String s -> LigatureValue.String s
    | Element.Word w -> LigatureValue.Word(Word w)
    | Element.Call(w, []) -> LigatureValue.Word(Word w)

let handleQuote (names: string list) (quote: Element list) : LigatureValue =
    let res = List.map (fun element -> elementToValue element) quote

    // let res =
    //     List.map
    //         (fun expression ->
    //             match expression with
    //             | Expression.Int i -> LigatureValue.Int i
    //             | Expression.String s -> LigatureValue.String s
    //             | Expression.Word w -> LigatureValue.Word(Word(w))
    //             | Expression.Network n -> LigatureValue.Network(n)
    //             | Expression.Bytes b -> LigatureValue.Bytes b
    //             | Expression.Slot s -> LigatureValue.Slot s
    //             | Expression.Quote q -> failwith "TODO")//LigatureValue.Quote q)
    //         res

    LigatureValue.Quote(names, res) //({ parameters = []; value = res })

let handleNetwork (network: (Element * Element * Element) list) : Network =
    let res: Set<Statement> =
        (List.map
            (fun (entity, attribute, value) ->
                match (entity, attribute, value) with
                | (Element.Word(entity), Element.Word(attribute), value) ->
                    let value =
                        match value with
                        | Element.Word w -> LigatureValue.Word(Word w)
                        | Element.Int i -> LigatureValue.Int i
                        | Element.String s -> LigatureValue.String s
                        | Element.Slot s -> LigatureValue.Slot s
                        | Element.Quote(p, q) -> handleQuote p q
                        | _ -> failwith "TODO"

                    (PatternWord.Word(Word(entity)), PatternWord.Word(Word(attribute)), value)
                | _ -> failwith "TODO")
            network)
        |> Set.ofSeq

    res

//     let entityDescriptions =
//         List.map (fun entityDescription -> expressEntityDescription entityDescription) entityDescriptions

//     (entity, entityDescriptions)

// let expressNetwork (values: NetworkRoot list) =
//     let res = List.map (fun networkRoot -> expressNetworkRoot networkRoot) values
//     Expression.Network res

// let handleAssocArray (declarations: list<string * Element>) =
//     let res =
//         List.map (fun (name, value) -> (name, (expressElement value))) declarations

//     Expression.AssocArray res

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
            express tail (List.append expressions [ Expression.Call(Word(w), { parameterNames = []; quote = [] }) ])
        | _ -> failwith "TODO"
