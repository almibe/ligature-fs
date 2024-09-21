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

type ParserError = string

let readComma: Nibbler<Token, unit> =
    fun state ->
        match readOffset 0 state with
        | Some Token.Comma -> Ok({ state with offset = state.offset + 1 }, ())
        | _ -> Error NoMatch

let readBinaryOperator: Nibbler<Token, Operator> =
    fun state ->
        match readOffset 0 state with
        | Some Token.ConceptConjunction -> Ok({ state with offset = state.offset + 1 }, Conjuntion)
        | Some _ -> Error NoMatch
        | None -> Error NoMatch

let readConceptName: Nibbler<Token, Node> =
    fun state ->
        match read 3 state with
        | [| Some(Token.Name concept); None; None |] -> Ok({ state with offset = state.offset + 1 }, Node.Name concept)
        | [| Some(Token.Negation); Some(Token.Name concept); None |] ->
            Ok({ state with offset = state.offset + 2 }, Node.NotName concept)
        | [| Some(Token.Name concept); Some(operator); _ |] ->
            match operator with
            | Token.Comma -> Ok({ state with offset = state.offset + 1 }, Node.Name concept)
            | _ -> Error NoMatch
        | [| Some(Token.Negation); Some(Token.Name concept); Some(operator) |] ->
            match operator with
            | Token.Comma -> Ok({ state with offset = state.offset + 2 }, Node.NotName concept)
            | _ -> Error NoMatch
        | _ -> failwith "TODO"

let readConceptExpression: Nibbler<Token, Node> =
    fun state ->
        match readConceptName state with
        | Ok(state, concept) ->
            match (readComma state, readBinaryOperator state) with
            | (Ok(state, _), Error _) -> Ok(state, concept)
            | (_, _) -> Error NoMatch
        | Error err -> Error err

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

let expressConcept (node: Node) : Result<ConceptExpression, ParserError> =
    match node with
    | Node.Name name -> Ok(AtomicConcept name)
    | Node.NotName name -> Ok(Not { concept = AtomicConcept name })
    | Node.BinaryOperator(left, operator, right) -> failwith "Not Implemented"
    | Node.NotBinaryOperator(left, operator, right) -> failwith "Not Implemented"
    | Node.UnaryPredicate(_, _) -> Error "Unexpected Uniary Predicate when Parsing Concept."

let express (nodes: Node list) : Result<KnowledgeBase, ParserError> =
    List.fold
        (fun state node ->
            match state with
            | Ok kb ->
                match node with
                | Node.UnaryPredicate(individual, concept) ->
                    match expressConcept concept with
                    | Ok concept ->
                        Ok(
                            Set.add
                                (UnaryPredicate
                                    { symbol = individual
                                      concept = concept })
                                kb
                        )
                    | Error _ -> failwith "TODO"
                | Node.Name _ -> Error "Unexpected name element."
                | Node.NotName _ -> Error "Unexpected not name element."
                | Node.BinaryOperator(_, _, _) -> failwith "Not Implemented"
                | Node.NotBinaryOperator(_, _, _) -> failwith "Not Implemented"
            | Error err -> Error err)
        (Ok emptyKB)
        nodes

let read (input: string) : Result<Node list, ParserError> =
    match tokenize input with
    | Ok res -> parse res
    | Error err -> Error err
