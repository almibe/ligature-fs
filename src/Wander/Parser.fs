// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module rec Ligature.Wander.Parser

open Lexer
open FsToolkit.ErrorHandling
open Model
open Nibblers
open Ligature
open System.Collections

[<RequireQualifiedAccess>]
type Element =
    | NamePath of string list
    | Grouping of Element list
    | Application of Element list
    | String of string
    | Int of bigint
    | Bytes of byte array
    | Bool of bool
    | Identifier of Ligature.Identifier
    | Array of Element list
    | Let of string * Element
    | Match of Element * (Element * Element) list
    | Lambda of string list * Element
    | Record of (string * Element) list
    | Dataset of DatasetRoot list
    | Pipe
    | Colon

and EntityDescription = Element * Element list

and DatasetRoot = Element * EntityDescription list

let nameStrNibbler (gaze: Gaze.Gaze<Token>) : Result<string, Gaze.GazeError> =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.Name(value)) -> Ok(value)
            | _ -> Error Gaze.GazeError.NoMatch)
        gaze

let nameNib (gaze: Gaze.Gaze<Token>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.Name(name)) -> Ok(Element.NamePath([ name ]))
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let namePathNib =
    Gaze.map (repeatSep nameStrNibbler Token.Dot) (fun namePath -> Element.NamePath namePath)

let readAssignment gaze =
    Gaze.attempt
        (fun gaze ->
            result {
                let! name = Gaze.attempt nameStrNibbler gaze
                let! _ = Gaze.attempt (take Token.EqualsSign) gaze
                let! v = elementNib gaze
                return Element.Let(name, v)
            })
        gaze

let conditionsNibbler (gaze: Gaze.Gaze<Token>) =
    result {
        let! _ = Gaze.attempt (take Token.Asterisk) gaze
        let! pattern = Gaze.attempt patternNib gaze
        let! _ = Gaze.attempt wideArrowNib gaze
        let! body = Gaze.attempt patternMatchBodyNib gaze
        return (pattern, body)
    }

let readMatch gaze =
    Gaze.attempt
        (fun gaze ->
            result {
                let! _ = Gaze.attempt (take Token.MatchKeyword) gaze
                let! expression = Gaze.attempt patternMatchBodyNib gaze
                let! conditions = Gaze.attempt (repeat conditionsNibbler) gaze
                return Element.Match(expression, conditions)
            })
        gaze

let readPipe = Gaze.map (take Token.Pipe) (fun _ -> Element.Pipe)

let colonNib = Gaze.map (take Token.Colon) (fun _ -> Element.Colon)

let lambdaNib gaze =
    Gaze.attempt
        (fun gaze ->
            result {
                let! _ = Gaze.attempt (take Token.Lambda) gaze
                let! parameters = Gaze.attempt (repeat nameStrNibbler) gaze
                let! _ = Gaze.attempt (take Token.Arrow) gaze
                let! body = Gaze.attempt elementNib gaze
                return Element.Lambda(parameters, body)
            })
        gaze

let readInteger (gaze: Gaze.Gaze<Token>) =
    Gaze.attempt
        (fun gaze ->
            match Gaze.next gaze with
            | Ok(Token.Int(i)) -> Ok(Element.Int(i))
            | _ -> Error(Gaze.GazeError.NoMatch))
        gaze

let applicationNib (gaze: Gaze.Gaze<Token>) =
    Gaze.map (repeatMulti applicationInnerNib) (fun elements -> Element.Application(elements)) gaze

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

let arrayNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenSquare) gaze
        let! values = Gaze.attempt (optional (repeatSep elementNib Token.Comma)) gaze
        let! _ = Gaze.attempt (take Token.CloseSquare) gaze
        return Element.Array(values)
    }

let groupingNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenParen) gaze
        let! values = Gaze.attempt (optional (repeatSep elementNib Token.Comma)) gaze
        let! _ = Gaze.attempt (take Token.CloseParen) gaze
        return Element.Grouping(values)
    }

let rec readEntityDescription gaze : Result<EntityDescription, Gaze.GazeError> =
    result {
        let! attribute = Gaze.attempt readIdentifier gaze //TODO only match Identifier or Name
        let! values = Gaze.attempt readValues gaze //TODO match single Value or List of Values
        return (attribute, values)
    }

let singleEntityDescriptNib gaze =
    result {
        let! entity = Gaze.attempt readIdentifier gaze //TODO only match Identifier or Name
        let! entityDescriptions = readEntityDescription gaze //Gaze.attempt (repeat readEntityDescription) gaze
        return DatasetRoot(entity, [ entityDescriptions ])
    }

let datasetRootNib (gaze: Gaze.Gaze<Token>) : Result<DatasetRoot, Gaze.GazeError> =
    let singleEntityDescription = Gaze.attempt singleEntityDescriptNib gaze

    match singleEntityDescription with
    | Ok _ -> singleEntityDescription
    | _ ->
        result {
            let! entity = Gaze.attempt readIdentifier gaze //TODO only match Identifier or Name
            let! _ = Gaze.attempt (take Token.OpenBrace) gaze
            let! entityDescriptions = (repeatSep readEntityDescription Token.Comma) gaze //Gaze.attempt (repeat readEntityDescription) gaze
            let! _ = Gaze.attempt (take Token.CloseBrace) gaze
            return DatasetRoot(entity, entityDescriptions)
        }

let datasetNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenBrace) gaze
        let! datasetInternals = (optional (repeatSep datasetRootNib Token.Comma)) gaze
        let! _ = Gaze.attempt (take Token.CloseBrace) gaze
        return Element.Dataset(datasetInternals)
    }

let declarationsNib (gaze: Gaze.Gaze<Token>) =
    result {
        let! name = Gaze.attempt nameStrNibbler gaze
        let! _ = Gaze.attempt equalSignNib gaze
        let! expression = Gaze.attempt elementNib gaze
        return (name, expression)
    }

let recordNib (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    result {
        let! _ = Gaze.attempt (take Token.OpenBrace) gaze
        let! declarations = (optional (repeatSep declarationsNib Token.Comma)) gaze
        let! _ = Gaze.attempt (take Token.CloseBrace) gaze
        return Element.Record(declarations)
    }

/// Read the next Element from the given instance of Gaze<Token>
let readValue (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Bytes(value)) -> Ok(Element.Bytes(value))
    | Ok(Token.Int(value)) -> Ok(Element.Int value)
    | Ok(Token.Bool(value)) -> Ok(Element.Bool value)
    | Ok(Token.Identifier(value)) -> Ok(Element.Identifier value)
    | Ok(Token.StringLiteral(value)) -> Ok(Element.String value)
    | _ -> Error(Gaze.GazeError.NoMatch)

let rec readValueList (elements: Element list) (gaze: Gaze.Gaze<Token>) : Result<Element list, Gaze.GazeError> =
    let next = Gaze.next gaze

    if next = Ok Token.CloseSquare then
        Ok elements
    else
        let elements =
            match next with
            | Ok(Token.Identifier i) -> List.append elements [ (Element.Identifier i) ]
            | Ok(Token.StringLiteral s) -> List.append elements [ (Element.String s) ]
            | Ok(Token.Bytes b) -> List.append elements [ (Element.Bytes b) ]
            | Ok(Token.Int i) -> List.append elements [ (Element.Int i) ]
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
    | Ok(Token.Bool(value)) -> Ok([ Element.Bool value ])
    | Ok(Token.Identifier(value)) -> Ok([ Element.Identifier value ])
    | Ok(Token.StringLiteral(value)) -> Ok([ Element.String value ])
    | Ok(Token.OpenSquare) -> readValueList [] gaze
    | _ -> Error(Gaze.GazeError.NoMatch)

let readIdentifier (gaze: Gaze.Gaze<Token>) : Result<Element, Gaze.GazeError> =
    let next = Gaze.next gaze

    match next with
    | Error(err) -> Error err
    | Ok(Token.Identifier(value)) -> Ok(Element.Identifier value)
    | _ -> Error(Gaze.GazeError.NoMatch)

let patternMatchBodyNib =
    takeFirst [ namePathNib; readValue; groupingNib; applicationNib ]

let patternNib = takeFirst [ namePathNib; readValue ]

let applicationInnerNib =
    takeFirst
        [ readPipe
          colonNib
          readValue
          namePathNib
          arrayNib
          datasetNib
          recordNib
          lambdaNib
          groupingNib
          readMatch ]

let elementNib =
    takeFirst
        [ readAssignment
          applicationNib
          namePathNib
          readValue
          arrayNib
          datasetNib
          recordNib
          lambdaNib
          groupingNib
          readMatch ]

let scriptNib = repeatSep elementNib Token.Comma

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

let expressArray values =
    let res = List.map (fun value -> expressElement value) values
    Expression.Array res

let expressEntityDescription entityDescription =
    let (attribute, values) = entityDescription
    ((expressElement attribute), (List.map (fun value -> expressElement value) values))

let expressDatasetRoot (datasetRoot: DatasetRoot) =
    let (entity, entityDescriptions) = datasetRoot
    let entity = expressElement entity

    let entityDescriptions =
        List.map (fun entityDescription -> expressEntityDescription entityDescription) entityDescriptions

    (entity, entityDescriptions)

let expressDataset (values: DatasetRoot list) =
    let res = List.map (fun datasetRoot -> expressDatasetRoot datasetRoot) values
    Expression.Dataset res

let expressGrouping values =
    let res = List.map (fun value -> expressElement value) values
    Expression.Grouping res

let handleRecord (declarations: list<string * Element>) =
    let res =
        List.map (fun (name, value) -> (name, (expressElement value))) declarations

    Expression.Record res

let handleLambda (parameters: string list) body =
    Expression.Lambda(parameters, (expressElement body))

let handleMatch (expression: Element) (conditionals: list<Element * Element>) =
    let expression = expressElement expression

    let conditionals =
        List.map (fun (condition, body) -> ((expressElement condition), (expressElement body))) conditionals

    Expression.Match(expression, conditionals)

let expressApplication elements =
    let parts = new Generic.List<Expression list>()
    let currentPart = new Generic.List<Expression>()

    List.iter
        (fun element ->
            match element with
            | Element.Pipe ->
                let part = List.ofSeq currentPart
                parts.Add part
                currentPart.Clear()
            | el -> currentPart.Add(expressElement el))
        elements

    parts.Add(List.ofSeq currentPart)

    List.fold
        (fun expr application -> List.append application [ expr ] |> Expression.Application)
        (Expression.Application(parts[0]))
        (List.ofSeq parts).Tail

let express (elements: Element list) : Expression list =
    List.map (fun element -> expressElement element) elements

let rec expressElement (element: Element) =
    match element with
    | Element.Int value -> Expression.Int value
    | Element.Bool value -> Expression.Bool value
    | Element.NamePath namePath -> Expression.NamePath namePath
    | Element.String value -> Expression.String value
    | Element.Identifier id -> Expression.Identifier id
    | Element.Let(name, value) -> Expression.Let(name, (expressElement value))
    | Element.Array values -> expressArray values
    | Element.Grouping elements -> expressGrouping elements
    | Element.Application elements -> expressApplication elements
    | Element.Record declarations -> handleRecord declarations
    | Element.Lambda(parameters, body) -> handleLambda parameters body
    | Element.Match(expression, conditionals) -> handleMatch expression conditionals
    | Element.Pipe -> failwith "Not Implemented"
    | Element.Bytes(bytes) -> Expression.Bytes(bytes)
    | Element.Dataset(value) -> expressDataset value
    | Element.Colon -> Expression.Colon
