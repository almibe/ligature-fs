// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Parser

open Tokenizer
open TinyDL.Main
open Nibblers

// let identifierNib (gaze: Gaze.Gaze<Token>) =
//     Gaze.attempt
//         (fun gaze ->
//             match Gaze.next gaze with
//             | Ok(Token.Name(name)) -> Ok(WanderValue.Symbol(name))
//             | _ -> Error(Gaze.GazeError.NoMatch))
//         gaze

// // let readNameStr (gaze: Gaze.Gaze<Token>) : Result<string, Gaze.GazeError> =
// //     let next = Gaze.next gaze

// //     match next with
// //     | Error(err) -> Error err
// //     | Ok(Token.Name(value)) -> Ok value
// //     | _ -> Error(Gaze.GazeError.NoMatch)

// let readSymbol (gaze: Gaze.Gaze<Token>) : Result<WanderValue, Gaze.GazeError> =
//     let next = Gaze.next gaze

//     match next with
//     | Error(err) -> Error err
//     | Ok(Token.Symbol(value)) -> Ok(WanderValue.Symbol(value))
//     | _ -> Error(Gaze.GazeError.NoMatch)

// let quoteNib (gaze: Gaze.Gaze<Token>) : Result<WanderValue, Gaze.GazeError> =
//     result {
//         let! _ = Gaze.attempt (take Token.OpenSquare) gaze
//         let! values = Gaze.attempt (optional (repeat valueNib)) gaze
//         let! _ = Gaze.attempt (take Token.CloseSquare) gaze
//         return WanderValue.Quote(values)
//     }

// let expressionNib (gaze: Gaze.Gaze<Token>) : Result<WanderValue, Gaze.GazeError> =
//     result {
//         let! _ = Gaze.attempt (take Token.OpenParen) gaze
//         let! values = Gaze.attempt (optional (repeat valueNib)) gaze
//         let! _ = Gaze.attempt (take Token.CloseParen) gaze
//         return WanderValue.Expression(values)
//     }

// // let argumentNib (gaze: Gaze.Gaze<Token>) : Result<(string * Identifier), Gaze.GazeError> =
// //     let entity = patternNib gaze
// //     let attribute = patternNib gaze

// //     let value =
// //         match Gaze.check valueNib gaze with
// //         | Ok(_) -> valueNib gaze
// //         | Error(_) -> quoteNib gaze

// //     match (entity, attribute, value) with
// //     | (Ok(Identifier.Name(name)), Ok(a), Ok(v)) -> Ok(name, v)
// //     | _ -> Error(Gaze.NoMatch)

// let statementNib (gaze: Gaze.Gaze<Token>) : Result<(WanderValue * WanderValue * WanderValue), Gaze.GazeError> =
//     let entity = patternNib gaze
//     let attribute = patternNib gaze

//     let value =
//         match Gaze.check valueNib gaze with
//         | Ok(_) -> valueNib gaze
//         | Error(_) -> quoteNib gaze

//     match (entity, attribute, value) with
//     | (Ok(e), Ok(a), Ok(v)) -> Ok(e, a, v)
//     | _ -> Error(Gaze.NoMatch)

// let networkNib (gaze: Gaze.Gaze<Token>) : Result<WanderValue, Gaze.GazeError> =
//     result {
//         let! _ = Gaze.attempt (take Token.OpenBrace) gaze
//         let! statements = (optional (repeatSep statementNib Token.Comma)) gaze
//         let! _ = Gaze.attempt (take Token.CloseBrace) gaze
//         return WanderValue.Network(handleNetwork statements)
//     }

// let patternNib (gaze: Gaze.Gaze<Token>) : Result<WanderValue, Gaze.GazeError> =
//     match Gaze.next gaze with
//     | Error(err) -> Error err
//     | Ok(Token.Symbol(value)) -> Ok(WanderValue.Symbol(value))
//     //    | Ok(Token.Slot(value)) -> Ok(WanderValue.Slot(value))
//     | Ok(Token.StringLiteral(value)) -> Ok(WanderValue.Symbol(Symbol(value)))
//     | _ -> Error(Gaze.GazeError.NoMatch)

let unaryPredicateNib (gaze: Gaze.Gaze<Token>) : Result<KnowledgeBase, Gaze.GazeError> =
    match (Gaze.next gaze, Gaze.next gaze, Gaze.next gaze) with
    | Ok(Token.Name individual), Ok(Token.Colon), Ok(Token.Name concept) ->
        Ok(
            emptyTBox,
            Set.ofList
                [ UnaryPredicate
                      { symbol = individual
                        concept = AtomicConcept concept } ]
        )
    | _ -> Error Gaze.GazeError.NoMatch

let atomicConceptNib (gaze: Gaze.Gaze<Token>) : Result<KnowledgeBase, Gaze.GazeError> =
    match (Gaze.next gaze) with
    | Ok(Token.Name individual) -> Ok(Set.ofList [ AtomicConcept individual ], emptyABox)
    | _ -> Error Gaze.GazeError.NoMatch

let conceptEquivNib (gaze: Gaze.Gaze<Token>) : Result<KnowledgeBase, Gaze.GazeError> =
    match (Gaze.next gaze, Gaze.next gaze, Gaze.next gaze) with
    | Ok(Token.Name left), Ok(Token.Equiv), Ok(Token.Name right) ->
        Ok(
            Set.ofList
                [ Equivalence
                      { left = AtomicConcept left
                        right = AtomicConcept right } ],
            emptyABox
        )
    | _ -> Error Gaze.GazeError.NoMatch

let conceptInclusionNib (gaze: Gaze.Gaze<Token>) : Result<KnowledgeBase, Gaze.GazeError> =
    match (Gaze.next gaze, Gaze.next gaze, Gaze.next gaze) with
    | Ok(Token.Name left), Ok(Token.ConceptInclusion), Ok(Token.Name right) ->
        Ok(
            Set.ofList
                [ Subsumption
                      { subsumee = AtomicConcept left
                        subsumer = AtomicConcept right } ],
            emptyABox
        )
    | _ -> Error Gaze.GazeError.NoMatch

let expressionNib =
    takeFirst [ unaryPredicateNib; conceptEquivNib; conceptInclusionNib; atomicConceptNib ] //quoteNib; expressionNib; ; networkNib ]

let valueNib = takeFirst [ atomicConceptNib ] //quoteNib; expressionNib; ; networkNib ]

// // let rec readValueList (elements: Pattern list) (gaze: Gaze.Gaze<Token>) : Result<Pattern list, Gaze.GazeError> =
// //     let next = Gaze.next gaze

// //     if next = Ok Token.CloseSquare then
// //         Ok elements
// //     else
// //         let elements =
// //             match next with
// //             | Ok(Token.Symbol w) -> List.append elements [ (Pattern.Symbol(w)) ]
// //             | Ok(Token.StringLiteral s) -> List.append elements [ (Pattern.Symbol(Symbol s)) ]
// //             | Ok(Token.Slot s) -> List.append elements [ (Pattern.Slot s) ]

// //         match Gaze.peek gaze with
// //         | Ok Token.CloseSquare ->
// //             (Gaze.next gaze |> ignore)
// //             Ok elements
// //         | Ok Token.Comma ->
// //             (Gaze.next gaze |> ignore)
// //             readValueList elements gaze

// // let readSlot (gaze: Gaze.Gaze<Token>) : Result<Pattern, Gaze.GazeError> =
// //     let next = Gaze.next gaze

// //     match next with
// //     | Error(err) -> Error err
// //     | Ok(Token.Slot(value)) -> Ok(Pattern.Slot value)
// //     | _ -> Error(Gaze.GazeError.NoMatch)

// //let patternMatchBodyNib = takeFirst [ networkNib; identifierNib; quoteNib ]

// //let patternNib = takeFirst [ networkNib ]

// let elementNib = takeFirst [ expressionNib; networkNib ]

let scriptNib = repeat expressionNib //valueNib

type ParserError = string

/// <summary></summary>
/// <param name="tokens">The list of Tokens to be parsered.</param>
/// <returns>The AST created from the token list of an Error.</returns>
let parse (tokens: Token list) : Result<KnowledgeBase, ParserError> =
    let tokens =
        List.filter
            (fun token ->
                match token with
                | Token.WhiteSpace(_)
                | Token.NewLine(_) -> false
                | _ -> true)
            tokens

    if tokens.IsEmpty then
        Ok(Set.empty, Set.empty)
    else
        let gaze = Gaze.fromList tokens

        match Gaze.attempt scriptNib gaze with
        | Ok res ->
            if Gaze.isComplete gaze then
                Ok(
                    List.fold
                        (fun (stateTBox, stateABox) (tBox, aBox) ->
                            ((Set.union stateTBox tBox), (Set.union stateABox aBox)))
                        emptyKB
                        res
                )
            else
                Error $"Failed to parse completely. {Gaze.remaining gaze}"
        | Error err -> Error $"Failed to parse.\n{err.ToString()}"

// let parseString (input: string) =
//     match tokenize input with
//     | Ok tokens -> parse tokens
//     | Error err -> error "Could not parse input." None //error $"Could not match from {gaze.offset} - {(Gaze.remaining gaze)}." None //TODO this error message needs updated

// // let elementToValue (element: Identifier) : Identifier =
// //     match element with
// //     | Identifier.Int i -> Identifier.Int i
// //     | Identifier.Bytes b -> Identifier.Bytes b
// //     | Identifier.Network n -> Identifier.Network(handleNetwork n)
// //     | Identifier.Quote p -> handleQuote p
// //     | Identifier.Expression e -> handleExpression e
// //     | Identifier.Slot s -> Identifier.Slot s
// //     | Identifier.String s -> Identifier.String s
// //     | Identifier.Name n -> Identifier.Name(Name n)

// // let handleQuote (quote: Identifier list) : Identifier =
// //     List.map (fun element -> elementToValue element) quote |> Identifier.Quote

// // let handleExpression (expression: Identifier list) : Identifier =
// //     List.map (fun element -> elementToValue element) expression
// //     |> Identifier.Expression

// let handleNetwork (network: (WanderValue * WanderValue * WanderValue) list) : Network =
//     let res: Set<Statement> = (List.map (elementTupleToStatement) network) |> Set.ofSeq
//     res

// let elementTupleToStatement ((e, a, v): (WanderValue * WanderValue * WanderValue)) : (Symbol * Symbol * Symbol) =
//     let entity =
//         match e with
//         | WanderValue.Symbol p -> p
//         //        | WanderValue.Slot s -> failwith "TODO" //Pattern.Slot s
//         | _ -> failwith "Error - unexpected Entity."

//     let attribute =
//         match a with
//         | WanderValue.Symbol p -> p
//         //        | WanderValue.Slot s -> failwith "TODO" //Pattern.Slot s
//         | _ -> failwith "Error - unexpected Attribute."

//     let value =
//         match v with
//         | WanderValue.Symbol p -> p
//         //       | WanderValue.Slot s -> failwith "TODO" //Pattern.Slot s
//         | _ -> failwith "Error - unexpected Value."

//     (entity, attribute, value)

// let expressExpression (elements: WanderValue list) : Element =
//     //    let res = List.map (fun element -> elementToValue element) elements
//     Element.Expression elements

// let rec express (elements: WanderValue list) (expressions: Element list) : Element list =
//     match elements with
//     | [] -> expressions
//     | WanderValue.Network(network) :: tail ->
//         express tail (List.append expressions [ Element.Network(defaultNetwork, network) ])
//     | WanderValue.Symbol(name) :: WanderValue.Network(network) :: tail ->
//         express tail (List.append expressions [ Element.Network(name, network) ])
//     | WanderValue.Expression e :: tail -> express tail (List.append expressions [ expressExpression e ])
//     //| Identifier.NetworkName n -> express tail (List.append expressions [ Command.NetworkName n ])
//     | _ -> failwith "Error - unexpected token."
