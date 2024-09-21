// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.NewParser

open Tokenizer
open TinyDL.Main
open New

type ParserError = string

let readConceptExpression: Nibbler<Token, ConceptExpression> =
    fun state ->
        match read 3 state with
        | [| Some(Token.Name concept); None; None |] ->
            Ok({ state with offset = state.offset + 1 }, AtomicConcept concept)
        | [| Some(Token.Name concept); Some(Token.Comma); _ |] ->
            Ok({ state with offset = state.offset + 2 }, AtomicConcept concept)
        | [| Some(Token.Negation); Some(Token.Name concept); None |] ->
            Ok({ state with offset = state.offset + 2 }, Not { concept = AtomicConcept concept })
        | [| Some(Token.Negation); Some(Token.Name concept); Some(Token.Comma) |] ->
            Ok({ state with offset = state.offset + 3 }, Not { concept = AtomicConcept concept })
        | _ -> failwith "TODO"

let readUnaryPredicate (state: State<Token>) =
    match read 2 state with
    | [| Some(Token.Name individual); Some Token.Colon |] ->
        match readOffset 2 state with
        | Some(Token.Name concept) ->
            Ok(
                { state with offset = state.offset + 3 },
                UnaryPredicate
                    { symbol = individual
                      concept = AtomicConcept concept }
            )
        | Some(Token.Negation) ->
            match readOffset 3 state with
            | Some(Token.Name concept) ->
                Ok(
                    { state with offset = state.offset + 3 },
                    UnaryPredicate
                        { symbol = individual
                          concept = Not { concept = AtomicConcept concept } }
                )
            | _ -> failwith "TODO"
        | _ -> failwith "TODO"
    | _ -> Error(NoMatch)

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
        let state = fromList tokens

        match readUnaryPredicate state with
        | Ok(state, res) -> Ok((emptyTBox, Set.ofList [ res ]))
        | Error err -> failwith "TODO"

let read (input: string) : Result<KnowledgeBase, ParserError> =
    match tokenize input with
    | Ok res -> parse res
    | Error err -> Error err
