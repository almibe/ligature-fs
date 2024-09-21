// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.NewParser

open Tokenizer
open TinyDL.Main
open New

type Operator = | Conjuntion

[<RequireQualifiedAccess>]
type Node =
    | Name of string
    | NotName of string
    | BinaryOperator of Node * Operator * Node
    | NotBinaryOperator of Node * Operator * Node
    | UnaryPredicate of string * Node

type AST = Node list

type ParserError = string

let readConceptExpression: Nibbler<Token, Node> =
    fun state ->
        match read 3 state with
        | [| Some(Token.Name concept); None; None |] -> Ok({ state with offset = state.offset + 1 }, Node.Name concept)
        | [| Some(Token.Name concept); Some(Token.Comma); _ |] ->
            Ok({ state with offset = state.offset + 2 }, Node.Name concept)
        | [| Some(Token.Negation); Some(Token.Name concept); None |] ->
            Ok({ state with offset = state.offset + 2 }, Node.NotName concept)
        | [| Some(Token.Negation); Some(Token.Name concept); Some(Token.Comma) |] ->
            Ok({ state with offset = state.offset + 3 }, Node.NotName concept)
        | _ -> failwith "TODO"

let readUnaryPredicate: Nibbler<Token, Node> =
    fun (state: State<Token>) ->
        match read 2 state with
        | [| Some(Token.Name individual); Some Token.Colon |] ->
            match readConceptExpression { state with offset = state.offset + 2 } with
            | Ok(state, (res)) -> Ok(state, Node.UnaryPredicate(individual, res))
            | _ -> failwith "TODO"
        // match readOffset 2 state with
        // | Some(Token.Name concept) ->
        //     Ok(
        //         { state with offset = state.offset + 3 },
        //         UnaryPredicate
        //             { symbol = individual
        //               concept = AtomicConcept concept }
        //     )
        // | Some(Token.Negation) ->
        //     match readOffset 3 state with
        //     | Some(Token.Name concept) ->
        //         Ok(
        //             { state with offset = state.offset + 3 },
        //             UnaryPredicate
        //                 { symbol = individual
        //                   concept = Not { concept = AtomicConcept concept } }
        //         )
        //     | _ -> failwith "TODO"
        // | _ -> failwith "TODO"
        | _ -> Error(NoMatch)

/// <summary></summary>
/// <param name="tokens">The list of Tokens to be parsered.</param>
/// <returns>The AST created from the token list of an Error.</returns>
let parse (tokens: Token list) : Result<Node list, ParserError> =
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
        let state = fromList tokens

        match readUnaryPredicate state with
        | Ok(state, res) -> Ok [ res ]
        | Error err -> failwith "TODO"

let express (nodes: Node list) : Result<KnowledgeBase, ParserError> =
    List.fold
        (fun state node ->

            failwith "TODO")
        (Ok emptyKB)
        nodes

let read (input: string) : Result<Node list, ParserError> =
    match tokenize input with
    | Ok res -> parse res
    | Error err -> Error err
