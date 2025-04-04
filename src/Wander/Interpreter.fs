// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Interpreter

open Ligature.Model
open Model
open Tokenizer
open Parser

let parseMatchBody (body: Any list) : Result<(Any * Any) list, LigatureError> =
    List.chunkBySize 3 body
    |> List.map (fun value ->
        match value with
        | [ pattern; Any.Term(Term "->"); body ] -> pattern, body
        | _ -> failwith "TODO")
    |> Ok

let handleMatch (value: Any) (body: Any list) : Result<Any, LigatureError> =
    let rec check (body: (Any * Any) list) =
        match body with
        | [] -> error "No match" None
        | (cond, body) :: tail -> if value = cond then Ok body else check tail

    match parseMatchBody body with
    | Ok body -> check body
    | Error e -> Error e

let rec evalScript
    (actions: Fns)
    (bindings: Bindings)
    (variables: Variables)
    (script: Script)
    : Result<Any, LigatureError> =
    match script with
    | Any.Application(Term "defn", Any.Term name :: Any.Tuple args :: value) :: tail ->
        let args =
            List.map
                (fun value ->
                    match value with
                    | Any.Variable variable -> variable
                    | _ -> failwith "Parameters must be Variables.")
                args

        let bindings = Map.add name (args, value) bindings

        if tail = [] then
            Ok(Any.Tuple [])
        else
            evalScript actions bindings variables tail
    | Any.Application(Term "let", [ Any.Variable var; value ]) :: tail ->
        //TODO process value
        let variables = Map.add var value variables

        if tail = [] then
            Ok(Any.Tuple [])
        else
            evalScript actions bindings variables tail
    | Any.Application(Term "->", body) :: tail ->
        match body with
        | [] -> failwith "Invalid pipe call."
        | initialExpression :: remainingExpressions ->
            match executeExpression actions bindings variables initialExpression with
            | Ok initialValue ->
                List.fold
                    (fun state currentExpression ->
                        match state with
                        | Ok prevValue ->
                            match currentExpression with
                            | Any.Application(name, args) ->
                                let newApp = name, List.append args [ prevValue ]
                                executeApplication actions bindings variables newApp
                            | _ -> failwith "TODO"
                        | Error err -> Error err)
                    (Ok initialValue)
                    remainingExpressions
            | _ -> failwith "TODO"
    | [] -> Ok(Any.Tuple [])
    | head :: [] -> executeExpression actions bindings variables head
    | head :: tail ->
        match executeExpression actions bindings variables head with
        | Ok _ -> evalScript actions bindings variables tail
        | Error err -> Error err

and createFn (doc: string) (script: Script) examples pre post : Fn =
    Fn(
        { doc = doc
          examples = examples
          args = pre
          result = post },
        (fun actions bindings variables args -> evalScript actions bindings variables script)
    )

and lookupFn (actions: Fns) (action: Term) : Fn option =
    match Map.tryFind action actions with
    | Some action -> Some action
    | None -> None

and evalRecord (actions: Fns) (bindings: Bindings) (variables: Variables) (record: Record) : Record =
    Map.map
        (fun _ value ->
            match value with
            | Any.Application application ->
                match executeApplication actions bindings variables application with
                | Ok res -> res
                | Error err -> failwith $"Error: {err.UserMessage}"
            | other -> other)
        record

and evalLambda
    (fns: Fns)
    (bindings: Bindings)
    (variables: Variables)
    (args: Any list)
    (lambda: Lambda)
    : Result<Any, LigatureError> =
    let parameters, body = lambda

    if args.Length = parameters.Length then
        let variables =
            Seq.fold (fun state (name, value) -> Map.add name value state) variables (Seq.zip parameters args)

        evalScript fns bindings variables body
    else
        error "Invalid number of arguments." None

and executeApplication
    (actions: Fns)
    (bindings: Bindings)
    (variables: Variables)
    (application: Application)
    : Result<Any, LigatureError> =
    let fn, args = application

    let args =
        List.map
            (fun arg ->
                match executeExpression actions bindings variables arg with
                | Ok value -> value
                | Error err -> failwith $"Error: {err.UserMessage}")
            args

    match fn, args, bindings.TryFind fn, actions.TryFind fn with
    | Term "match", value :: body, _, _ -> handleMatch value body
    | _, _, Some lambda, _ -> evalLambda actions bindings variables args lambda
    | _, _, _, Some(Fn(_, fn)) ->
        fn
            actions
            bindings
            variables
            (List.map
                (fun value ->
                    match value with
                    | Any.Application application ->
                        match executeApplication actions bindings variables application with
                        | Ok res -> res
                        | Error err -> failwith $"Error: {err.UserMessage}"
                    | Any.Record record -> Any.Record(evalRecord actions bindings variables record)
                    | _ -> value)
                args)
    | _, _, None, None -> error $"Could not find function {fn}" None

and executeExpression
    (actions: Fns)
    (bindings: Bindings)
    (variables: Variables)
    (expression: Any)
    : Result<Any, LigatureError> =
    // let application = rewriteApplication expression

    match expression with
    | Any.Network network -> Ok(Any.Network network)
    | Any.Tuple tuple ->
        let tuple =
            List.map
                (fun value ->
                    match value with
                    | Any.Application app ->
                        match executeApplication actions bindings variables app with
                        | Ok value -> value
                        | _ -> failwith "TODO"
                    | Any.Variable variable ->
                        match Map.tryFind variable variables with
                        | Some value -> value
                        | _ -> failwith "TODO"
                    | Any.Tuple tuple ->
                        match executeExpression actions bindings variables (Any.Tuple tuple) with
                        | Ok value -> value
                        | _ -> failwith "TODO"
                    | value -> value)
                tuple

        Ok(Any.Tuple tuple)
    | Any.Record record -> Ok(Any.Record(evalRecord actions bindings variables record))
    | Any.Literal literal -> Ok(Any.Literal literal)
    | Any.Variable variable ->
        match variables.TryFind variable with
        | Some value -> Ok value
        | _ -> error $"Could not find {variable}" None
    | Any.Term term -> Ok(Any.Term term)
    | Any.Application application -> executeApplication actions bindings variables application
    | _ -> failwith "TODO"
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
