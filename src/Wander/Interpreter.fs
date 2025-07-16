// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Interpreter

open Ligature.Model
open Model
open Tokenizer
open Parser

let parseMatchBody (body: Expression list) : Result<(Expression * Expression) list, LigatureError> =
    List.chunkBySize 3 body
    |> List.map (fun value ->
        match value with
        | [ pattern; Expression.Term(Term "->"); body ] -> pattern, body
        | _ -> failwith "TODO")
    |> Ok

let handleMatch (value: Expression) (body: Expression list) : Result<Expression, LigatureError> =
    let rec check (body: (Expression * Expression) list) =
        match body with
        | [] -> error "No match" None
        | (cond, body) :: tail -> if value = cond then Ok body else check tail

    match parseMatchBody body with
    | Ok body -> check body
    | Error e -> Error e

let rec evalScript (actions: Fns) (variables: Variables) (script: Script) : Result<Expression, LigatureError> =
    let mutable variables = variables

    List.fold
        (fun state value ->
            if Result.isError state then
                state
            else
                match value with
                | None, expression -> executeExpression actions variables expression
                | Some variable, expression ->
                    match executeExpression actions variables expression with
                    | Ok res ->
                        variables <- Map.add variable res variables
                        Ok res
                    | Error err -> Error err)
        (Ok(Expression.Term(Term "")))
        script

and createFn (doc: string) (script: Script) examples pre post : Fn =
    Fn.Fn(
        { doc = doc
          examples = examples
          args = pre
          result = post },
        (fun actions variables args -> evalScript actions variables script)
    )

and lookupFn (actions: Fns) (action: Term) : Fn option =
    match Map.tryFind action actions with
    | Some action -> Some action
    | None -> None

and evalNode (actions: Fns) (variables: Variables) (node: Node) : Node =
    { name = node.name
      attributes =
        Map.map
            (fun _ value ->
                match executeExpression actions variables value with
                | Ok res -> res
                | Error err -> failwith $"Error: {err.UserMessage}")
            node.attributes
      children =
        List.map
            (fun value ->
                match executeExpression actions variables value with
                | Ok res -> res
                | Error err -> failwith $"Error: {err.UserMessage}")
            node.children }

and evalLambda
    (fns: Fns)
    (variables: Variables)
    (args: Expression list)
    (lambda: Lambda)
    : Result<Expression, LigatureError> =
    let parameters, body = lambda

    if args.Length = parameters.Length then
        let variables =
            Seq.fold (fun state (name, value) -> Map.add name value state) variables (Seq.zip parameters args)

        evalScript fns variables body
    else
        error "Invalid number of arguments." None

and vaToLambda variableApplication : Lambda = failwith "TODO"

and executeVariableApplication
    (actions: Fns)
    (variables: Variables)
    (application: VariableApplication)
    : Result<Expression, LigatureError> =
    let { variable = fn
          attributes = attributes
          children = args } =
        application

    let args =
        List.map
            (fun arg ->
                match executeExpression actions variables arg with
                | Ok value -> value
                | Error err -> failwith $"Error: {err.UserMessage}")
            args

    match variables.TryFind fn with
    | Some(Expression.VariableApplication lambda) -> evalLambda actions variables args (vaToLambda lambda)
    | Some(Expression.Term t) -> Ok(Expression.Term t)
    | Some(Expression.Lambda lambda) -> evalLambda actions variables args lambda
    | Some x -> failwith $"Unexpected value {x}"
    | None -> error $"Could not find function {fn}" None

and executeApplication (actions: Fns) (variables: Variables) (application: Node) : Result<Expression, LigatureError> =
    let { name = fn
          attributes = attributes
          children = args } =
        application

    match fn, args, actions.TryFind fn with
    //| Term "match", value :: body, _ -> handleMatch value body
    // | _, _, Some lambda, _ -> evalLambda actions variables args lambda
    | _, _, Some(Fn.Fn(_, fn)) ->
        let args =
            List.map
                (fun arg ->
                    match executeExpression actions variables arg with
                    | Ok value -> value
                    | Error err -> failwith $"Error: {err.UserMessage}")
                args

        fn
            actions
            variables
            (List.map
                (fun value ->
                    match value with
                    | Expression.Application node -> Expression.Application(evalNode actions variables node)
                    | _ -> value)
                args)
    | _, _, Some(Fn.Macro(_, fn)) -> fn actions variables args
    | _, _, None -> error $"Could not find function {fn}" None

and executeExpression
    (actions: Fns)
    (variables: Variables)
    (expression: Expression)
    : Result<Expression, LigatureError> =
    // let application = rewriteApplication expression

    match expression with
    | Expression.Assertions network -> Ok(Expression.Assertions network)
    // | Expression.Tuple tuple ->
    //     let tuple =
    //         List.map
    //             (fun value ->
    //                 match executeExpression actions bindings variables value with
    //                 | Ok res -> res
    //                 | Error err -> failwith err.UserMessage)
    //             tuple

    //     Ok(Expression.Tuple tuple)
    // | Expression.Set set ->
    //     let set =
    //         Set.map
    //             (fun value ->
    //                 match executeExpression actions bindings variables value with
    //                 | Ok res -> res
    //                 | Error err -> failwith err.UserMessage)
    //             set

    //     Ok(Expression.Set set)
    | Expression.Element literal -> Ok(Expression.Element literal)
    | Expression.Variable variable ->
        match variables.TryFind variable with
        | Some value -> Ok value
        | _ -> error $"Could not find {variable}" None
    | Expression.Term term -> Ok(Expression.Term term)
    | Expression.Application application -> executeApplication actions variables application
    | Expression.VariableApplication application -> executeVariableApplication actions variables application
    | Expression.NodeLiteral node -> Ok(Expression.NodeLiteral(evalNode actions variables node))
    | Expression.Assertion assertion -> failwith "TODO"
    // | Expression.Slot _ -> Ok expression
    | Expression.Comment _ -> failwith "Not Implemented"
    | Expression.Lambda _ -> failwith "Not Implemented"
    | Expression.ConceptExpr expr -> Ok(Expression.ConceptExpr expr)
    | Expression.Definitions _ -> failwith "Not Implemented"
//| _ -> failwith "TODO"
// match lookupFn actions action with
// | Some(Fn.Full(_, action)) -> action actions stack
// | Some(Fn.Stack(_, action)) ->
//     match action stack with
//     | Ok stack -> Ok(stack)
//     | Error err -> Error err
// | None -> error $"Could not find action {action}." None

let read (input: string) : Result<Script, LigatureError> =
    try
        match tokenize input with
        | Ok tokens -> parse tokens
        | Error _ -> error "Error tokenizing." None
    with x ->
        error $"Error reading {x}" None
