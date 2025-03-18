// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Interpreter

open Ligature.Model
open Model
open Tokenizer
open Parser

let rec evalScript (actions: Fns) (bindings: Bindings) (script: Script) : Result<Any, LigatureError> =
    match script with
    | [] -> Ok(Any.Network Set.empty)
    | [ head ] ->
        match head with
        | Expression.Assignment(_, _, _) -> Ok(Any.Network Set.empty)
        | Expression.Application application -> executeApplication actions bindings application
    | head :: tail ->
        match head with
        | Expression.Assignment(name, argNames, value) ->
            let bindings =
                match argNames, value with
                | [], value ->
                    Map.add name value bindings
                | args, Any.Lambda lambda -> failwith "TODO"
                | _ -> failwith "Invalid assignment"
            evalScript actions bindings tail
        | Expression.Application application ->
            match executeApplication actions bindings application with
            | Ok _ -> evalScript actions bindings tail
            | Error err -> Error err

and createFn (doc: string) (script: Script) examples pre post : Fn =
    Fn(
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

and rewriteApplication application =
    let mutable currentBlock = []
    let mutable prevBlock = []

    List.iter
        (fun value ->
            match value with
            | Any.Pipe ->
                currentBlock <- [ Any.Block [ Expression.Application(List.append currentBlock prevBlock) ] ]
                prevBlock <- currentBlock
                currentBlock <- []
            | _ -> currentBlock <- List.append currentBlock [ value ])
        application

    List.append currentBlock prevBlock

and evalRecord (actions: Fns) (bindings: Bindings) (record: Record) : Record =
    Map.map
        (fun _ value ->
            match value with
            | Any.Block block ->
                match evalScript actions bindings block with
                | Ok res -> res
                | Error err -> failwith $"Error: {err.UserMessage}"
            | other -> other)
        record

and executeApplication (actions: Fns) (bindings: Bindings) (application: Any list) : Result<Any, LigatureError> =
    let application = rewriteApplication application

    match application with
    | [ Any.Network network ] -> Ok(Any.Network network)
    | [ Any.Quote quote ] -> Ok(Any.Quote quote)
    | [ Any.Record record ] -> Ok(Any.Record(evalRecord actions bindings record))
    | [ Any.Literal literal ] -> Ok(Any.Literal literal)
    | Any.Term fn :: tail ->
        match bindings.TryFind fn, actions.TryFind fn with
        | Some binding, _ -> 
            
            failwith "TODO"
        | None, Some(Fn(_, fn)) ->
            fn
                actions
                bindings
                (List.map
                    (fun value ->
                        match value with
                        | Any.Block block ->
                            match evalScript actions bindings block with
                            | Ok res -> res
                            | Error err -> failwith $"Error: {err.UserMessage}"
                        | Any.Record record -> Any.Record(evalRecord actions bindings record)
                        | _ -> value)
                    tail)
        | None, None -> error $"Could not find function {fn}" None
    | [ Any.Block block ] ->
        match evalScript actions bindings block with
        | Ok res -> Ok res
        | Error err -> failwith $"Error: {err.UserMessage}"
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
