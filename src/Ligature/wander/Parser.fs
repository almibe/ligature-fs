// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module rec Ligature.Wander.Parser

open Lexer
open FsToolkit.ErrorHandling
open Model
open Nibblers
open Ligature.Main
open Ligature.InMemoryNetwork

[<RequireQualifiedAccess>]
type Element =
    | Word of string
    | Quote of Element list
    | String of string
    | Int of bigint
    | Bytes of byte array
    | Slot of Slot
    | Definition of string * Element
    | AssocArray of (string * Element) list
    | Network of (Element * Element * Element) list
    | Colon

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

let readAssignment gaze =
    Gaze.attempt
        (fun gaze ->
            result {
                let! name = Gaze.attempt nameStrNibbler gaze
                let! _ = Gaze.attempt (take Token.EqualsSign) gaze
                let! v = elementNib gaze
                return Element.Definition(name, v)
            })
        gaze

let colonNib = Gaze.map (take Token.Colon) (fun _ -> Element.Colon)

let readInteger (gaze: Gaze.Gaze<Token>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.Int(i)) -> Ok(Element.Int(i))
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let equalSignNib (gaze: Gaze.Gaze<Token>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.EqualsSign) -> Ok(())
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
        return Element.Quote(values)
    }

// let rec readEntityDescription gaze : Result<EntityDescription, Gaze.GazeError> =
//     let res = result {
//         let! attribute = Gaze.attempt (takeFirst [ readIdentifier ]) gaze //TODO only match Identifier or Name
//         let! values = Gaze.attempt readValues gaze //TODO match single Value or List of Values
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

let triplesNib (gaze: Gaze.Gaze<Token>) : Result<(Element * Element * Element), Gaze.GazeError> =
    let entity = 
        match wordNib gaze with
        | Ok(Element.Word(word)) -> Ok(Element.Word(word))
        | _ -> failwith "TODO"

    let attribute = 
        match wordNib gaze with
        | Ok(Element.Word(word)) -> Ok(Element.Word(word))
        | _ -> failwith "TODO"

    let value = 
        match wordNib gaze with
        | Ok(Element.Word(word)) -> Ok(Element.Word(word))
        | _ -> failwith "TODO"

    match (entity, attribute, value) with
    | (Ok(e), Ok(a), Ok(v)) -> Ok(e, a, v)
    | _ -> failwith "TODO"

let networkNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenBrace) gaze
        let! datasetInternals = (optional (repeatSep triplesNib Token.Comma)) gaze
        let! _ = Gaze.attempt (take Token.CloseBrace) gaze
        return Element.Network(datasetInternals)
    }

let declarationsNib (gaze: Gaze.Gaze<Token>) =
    result {
        let! name = Gaze.attempt nameStrNibbler gaze
        let! _ = Gaze.attempt equalSignNib gaze
        let! expression = Gaze.attempt elementNib gaze
        return (name, expression)
    }

let assocArrayNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenSquare) gaze

        let! declarations = (optional (repeatSep declarationsNib Token.Comma)) gaze

        let! _ = Gaze.attempt (take Token.CloseSquare) gaze

        if List.isEmpty declarations then
            return Element.Quote(List.empty)
        else
            return Element.AssocArray(declarations)
    }

/// Read the next Element from the given instance of Gaze<Token>
let readValue (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Bytes(value)) -> Ok(Element.Bytes(value))
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
            | Ok(Token.Bytes b) -> List.append elements [ (Element.Bytes b) ]
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
let readValues (gaze: Gaze.Gaze<Token>) : Result<Element list, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Bytes(value)) -> Ok([ Element.Bytes(value) ])
    | Ok(Token.Int(value)) -> Ok([ Element.Int value ])
    | Ok(Token.Word(value)) -> Ok([ Element.Word value ])
    | Ok(Token.StringLiteral(value)) -> Ok([ Element.String value ])
    | Ok(Token.Slot(slot)) -> Ok([ Element.Slot slot ])
    | Ok(Token.OpenSquare) -> readValueList [] gaze
    | _ ->
        match Gaze.attempt wordNib gaze with
        | Ok res -> Ok([ res ])
        | Error err -> Error(Gaze.GazeError.NoMatch)

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

let applicationInnerNib =
    takeFirst [ colonNib; readValue; wordNib; assocArrayNib; quoteNib ]

let elementNib =
    takeFirst [ readAssignment; wordNib; networkNib; readValue; assocArrayNib; quoteNib; quoteNib ]

let scriptNib = repeat elementNib

/// <summary></summary>
/// <param name="tokens">The list of Tokens to be parsered.</param>
/// <returns>The AST created from the token list of an Error.</returns>
let parse (tokens: Token list) : Result<Element list, LigatureError> =
    let tokens =
        List.filter
            (fun token ->
                match token with
                | Token.Comment(_)
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

let expressQuote values =
    let res = List.map (fun value -> expressElement value) values
    Expression.Quote res

// let expressEntityDescription (entityDescription: EntityDescription) =
//     let (attribute, values) = entityDescription
//     (attribute, (List.map (fun value -> expressElement value) values))

let expressNetwork (network: (Element * Element * Element) list): Expression = 
    let res: Set<Triple> = (List.map (fun (entity, attribute, value) ->
        match (entity, attribute, value) with
        | (Element.Word(entity), Element.Word(attribute), Element.Word(value)) -> 
            (PatternWord.Word(Word(entity)), PatternWord.Word(Word(attribute)), Value.Word(Word(value)))
        | _ -> failwith "TODO") network) |> Set.ofSeq
    Expression.Network(networkOf res)

//     let entityDescriptions =
//         List.map (fun entityDescription -> expressEntityDescription entityDescription) entityDescriptions

//     (entity, entityDescriptions)

// let expressNetwork (values: NetworkRoot list) =
//     let res = List.map (fun networkRoot -> expressNetworkRoot networkRoot) values
//     Expression.Network res

let handleAssocArray (declarations: list<string * Element>) =
    let res =
        List.map (fun (name, value) -> (name, (expressElement value))) declarations

    Expression.AssocArray res

let express (elements: Element list) : Expression list =
    List.map (fun element -> expressElement element) elements

let rec expressElement (element: Element) =
    match element with
    | Element.Int value -> Expression.Int value
    | Element.Word namePath -> Expression.Word namePath
    | Element.String value -> Expression.String value
    | Element.Definition(name, value) -> Expression.Definition(name, (expressElement value))
    | Element.Quote elements -> expressQuote elements
    | Element.AssocArray declarations -> handleAssocArray declarations
    | Element.Bytes bytes -> Expression.Bytes bytes
    | Element.Network value -> expressNetwork value
    | Element.Colon -> Expression.Colon
    | Element.Slot slot -> Expression.Slot slot
