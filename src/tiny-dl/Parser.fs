// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Parser

open Tokenizer
open TinyDL.Main
open Nibblers

let atomicConceptNib (gaze: Gaze.Gaze<Token>) : Result<ConceptExpression, Gaze.GazeError> =
    match (Gaze.next gaze) with
    | Ok(Token.Name individual) -> Ok(AtomicConcept individual)
    | _ -> Error Gaze.GazeError.NoMatch

let unaryConceptExpressionNib (gaze: Gaze.Gaze<Token>) : Result<ConceptExpression, Gaze.GazeError> =
    match (Gaze.next gaze, Gaze.next gaze) with
    | (Ok Token.Negation, Ok(Token.Name individual)) -> Ok(Not { concept = AtomicConcept individual })
    | _ -> Error Gaze.GazeError.NoMatch

let conceptExpressionNib = takeFirst [ atomicConceptNib; unaryConceptExpressionNib ]

let unaryPredicateNib (gaze: Gaze.Gaze<Token>) : Result<KnowledgeBase, Gaze.GazeError> =
    match (Gaze.next gaze, Gaze.next gaze) with
    | Ok(Token.Name individual), Ok(Token.Colon) ->
        match Gaze.attempt conceptExpressionNib gaze with
        | Ok res -> Ok(emptyTBox, Set.ofList [ UnaryPredicate { symbol = individual; concept = res } ])
        | Error err -> Error err
    | _ -> Error Gaze.GazeError.NoMatch

let binaryPredicateNib (gaze: Gaze.Gaze<Token>) : Result<KnowledgeBase, Gaze.GazeError> =
    match
        (Gaze.next gaze, Gaze.next gaze, Gaze.next gaze, Gaze.next gaze, Gaze.next gaze, Gaze.next gaze, Gaze.next gaze)
    with
    | Ok(Token.OpenParen),
      Ok(Token.Name left),
      Ok(Token.Comma),
      Ok(Token.Name right),
      Ok(Token.CloseParen),
      Ok(Token.Colon),
      Ok(Token.Name role) ->
        Ok(
            emptyTBox,
            Set.ofList
                [ BinaryPredicate
                      { left = left
                        right = right
                        role = role } ]
        )
    | _ -> Error Gaze.GazeError.NoMatch

let conceptDefinitionNib (gaze: Gaze.Gaze<Token>) : Result<KnowledgeBase, Gaze.GazeError> =
    match (Gaze.next gaze, Gaze.next gaze, Gaze.next gaze) with
    | Ok(Token.Name left), Ok(Token.Definition), Ok(Token.Name right) ->
        Ok(
            Set.ofList
                [ Definition
                      { left = left
                        right = AtomicConcept right } ],
            emptyABox
        )
    | _ -> Error Gaze.GazeError.NoMatch

let conceptConjunctionNib (gaze: Gaze.Gaze<Token>) : Result<ConceptExpression, Gaze.GazeError> = failwith "TODO"
// match (Gaze.next gaze, Gaze.next gaze, Gaze.next gaze) with
// | Ok(Token.Name left), Ok(Token.ConceptConjunction), Ok(Token.Name right) ->
//     Ok(
//         Set.ofList
//             [ Conjunction
//                   { left = AtomicConcept left
//                     right = AtomicConcept right } ],
//         emptyABox
//     )
// | _ -> Error Gaze.GazeError.NoMatch

let conceptInclusionNib (gaze: Gaze.Gaze<Token>) : Result<KnowledgeBase, Gaze.GazeError> =
    match (Gaze.next gaze, Gaze.next gaze, Gaze.next gaze) with
    | Ok(Token.Name concept), Ok(Token.ConceptInclusion), Ok(Token.Name concept') ->
        match Gaze.peek gaze with
        | Ok(Token.Comma)
        | Error Gaze.NoMatch ->
            Ok(
                Set.ofList
                    [ Inclusion
                          { left = concept
                            right = AtomicConcept concept' } ],
                emptyABox
            )
        | _ ->
            // Ok(
            //     Set.ofList
            //         [ Inclusion
            //               { left = concept
            //                 right = Conjunction {
            //                     left = AtomicConcept left
            //                     right = AtomicConcept right
            //                 } } ],
            //     emptyABox
            // )
            failwith "TODO"
    | _ -> Error Gaze.GazeError.NoMatch


let expressionNib =
    takeFirst
        [ unaryPredicateNib
          binaryPredicateNib
          conceptDefinitionNib
          conceptInclusionNib ]

let scriptNib = repeatSep expressionNib Token.Comma //valueNib

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

let read (input: string) : Result<KnowledgeBase, ParserError> =
    match tokenize input with
    | Ok res -> parse res
    | Error err -> Error err
