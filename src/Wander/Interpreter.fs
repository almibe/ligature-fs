// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Interpreter

open Ligature.Model
open Model
open Tokenizer
open Parser

let rec evalScript (actions: Fns) (variables: Variables) (script: Script) : Result<Variables * Any, LigatureError> =
    match script with
    | [] -> Ok(variables, Any.Network Set.empty)
    | [ head ] ->
        match head with
        | Assignment(variable, value) -> failwith "TODO"
        | Application application -> executeApplication actions variables application
    | head :: tail ->
        match head with
        | Assignment(variable, value) -> failwith "TODO"
        | Application application ->
            match executeApplication actions variables application with
            | Ok(variables, _) -> evalScript actions variables tail
            | Error err -> Error err

and createFn (doc: string) (script: Script) examples pre post : Fn =
    Fn(
        { doc = doc
          examples = examples
          pre = pre
          post = post },
        (fun actions variables args -> evalScript actions variables script)
    )

and lookupFn (actions: Fns) (action: Term) : Fn option =
    match Map.tryFind action actions with
    | Some(action) -> Some(action)
    | None -> None

and executeApplication
    (actions: Fns)
    (variables: Variables)
    (application: Any list)
    : Result<Variables * Any, LigatureError> =
    match application with
    | [ Any.Network network ] -> Ok(variables, Any.Network network)
    | Any.Term fn :: tail ->
        match actions.TryFind fn with
        | Some(Fn(_, fn)) ->
            fn
                actions
                variables
                (List.map
                    (fun value ->
                        match value with
                        | Any.Block block ->
                            match evalScript actions variables block with
                            | Ok(_, res) -> res
                            | _ -> failwith "TODO"
                        | _ -> value)
                    tail)
        | None -> failwith "TODO"
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
