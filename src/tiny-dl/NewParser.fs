// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.NewParser

open Tokenizer
open New
open Main
open Ligature.Main

[<RequireQualifiedAccess>]
type ConceptExpressionNode =
    | Name of string
    | Not
    | Conjunction

[<RequireQualifiedAccess>]
type Node = UnaryPredicate of string * ConceptExpressionNode list

type ParserError = string

let readEnd: Nibbler<Token, unit> =
    fun state ->
        match readOffset 0 state with
        | None -> Ok(state, ())
        | _ -> Error NoMatch

let readComma: Nibbler<Token, unit> =
    fun state ->
        match readOffset 0 state with
        | Some Token.Comma -> Ok({ state with offset = state.offset + 1 }, ())
        | _ -> Error NoMatch

// let readBinaryOperator: Nibbler<Token, Operator> =
//     fun state ->
//         match readOffset 0 state with
//         | Some Token.ConceptConjunction -> Ok({ state with offset = state.offset + 1 }, Conjuntion)
//         | Some _ -> Error NoMatch
//         | None -> Error NoMatch

// let readConceptName: Nibbler<Token, Node> =
//     fun state ->
//         match read 3 state with
//         | [| Some(Token.Name concept); None; None |] -> Ok({ state with offset = state.offset + 1 }, Node.Name concept)
//         | [| Some(Token.Negation); Some(Token.Name concept); None |] ->
//             Ok({ state with offset = state.offset + 2 }, Node.NotName concept)
//         | [| Some(Token.Name concept); Some(operator); _ |] ->
//             match operator with
//             | Token.ConceptConjunction
//             | Token.Comma -> Ok({ state with offset = state.offset + 1 }, Node.Name concept)
//             | _ -> failwith "TODO"
//         | [| Some(Token.Negation); Some(Token.Name concept); Some(operator) |] ->
//             match operator with
//             | Token.Comma -> Ok({ state with offset = state.offset + 2 }, Node.NotName concept)
//             | _ -> Error NoMatch
//         | _ -> failwith "TODO"

let rec readConceptExpression: Nibbler<Token, ConceptExpressionNode list> =
    fun state ->
        let mutable cont = true
        let mutable result: ConceptExpressionNode list = []
        let mutable offset = 0
        let mutable error = false

        while cont do
            match readOffset offset state with
            | Some(token) ->
                match token with
                | Token.Comma ->
                    offset <- offset + 1
                    cont <- false
                | Token.Name name ->
                    offset <- offset + 1
                    result <- List.append result [ ConceptExpressionNode.Name name ]
                | Token.ConceptConjunction ->
                    offset <- offset + 1
                    result <- List.append result [ ConceptExpressionNode.Conjunction ]
                | Token.OpenParen -> failwith "Not Implemented"
                | Token.CloseParen -> failwith "Not Implemented"
                | Token.Exists -> failwith "Not Implemented"
                | Token.All -> failwith "Not Implemented"
                | Token.ConceptInclusion -> failwith "Not Implemented"
                | Token.ConceptDisjunction -> failwith "Not Implemented"
                | Token.Negation ->
                    offset <- offset + 1
                    result <- List.append result [ ConceptExpressionNode.Not ]
                | Token.Dot -> failwith "Not Implemented"
                | Token.Top -> failwith "Not Implemented"
                | Token.Bottom -> failwith "Not Implemented"
                | Token.Definition -> failwith "Not Implemented"
                | _ ->
                    cont <- false
                    error <- true
            | None -> cont <- false

        if List.isEmpty result || error then
            Error NoMatch
        else
            Ok(
                { state with
                    offset = state.offset + offset },
                result
            )

let readUnaryPredicate: Nibbler<Token, Node> =
    fun (state: State<Token>) ->
        match read 2 state with
        | [| Some(Token.Name individual); Some Token.Colon |] ->
            match readConceptExpression { state with offset = state.offset + 2 } with
            | Ok(state, res) -> Ok(state, Node.UnaryPredicate(individual, res))
            | Error err -> Error err
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
        let mutable state = fromList tokens
        let mutable result = []

        while not (isComplete state) do
            match readUnaryPredicate state with
            | Ok(state', res) ->
                result <- List.append result [ res ]
                state <- state'
            | Error err -> failwith "TODO" //Error $"Error parsing @ {state.offset} = {state.input[state.offset]}."

        Ok result

let expressConcept (nodes: ConceptExpressionNode list) : Result<ConceptExpression, ParserError> =
    let mutable state = fromList nodes
    let mutable result = Error "Could not express concept."
    let mutable cont = true

    while (not (isComplete state)) && cont do
        match readOffset 0 state with
        | None -> result <- Error "Unexpected Element when Parsing Concept."
        | Some(ConceptExpressionNode.Name name) ->
            cont <- false
            result <- Ok(AtomicConcept(Symbol name))
        | Some(ConceptExpressionNode.Not) ->
            match readOffset 1 state with
            | None ->
                result <- Error "Error expressing concept."
                cont <- false
            | Some(ConceptExpressionNode.Name concept) ->
                result <- Ok(Not { concept = AtomicConcept(Symbol concept) })
                cont <- false
            | Some _ ->
                result <- Error "Error expressing concept."
                cont <- false
        // | [ ConceptExpressionNode.Name left; ConceptExpressionNode.Conjunction; ConceptExpressionNode.Name right ] ->
        //     Ok(Conjunction { left = AtomicConcept left; right = AtomicConcept right })
        | _ -> failwith "TODO"

    result

let express (nodes: Node list) : Result<KnowledgeBase, ParserError> =
    List.fold
        (fun state node ->
            match state with
            | Ok kb ->
                match node with
                | Node.UnaryPredicate(individual, concept) ->
                    match expressConcept concept with
                    | Ok concept -> failwith "TODO"
                    // Ok(
                    //     Set.add
                    //         (UnaryPredicate
                    //             { symbol = individual
                    //             concept = concept })
                    //         kb
                    // )
                    | Error _ -> failwith "TODO"
            | Error err -> Error err)
        (Ok emptyKB)
        nodes

let read (input: string) : Result<Node list, ParserError> =
    match tokenize input with
    | Ok res -> parse res
    | Error err -> Error err
