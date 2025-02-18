// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Interpreter

open Ligature.Model
open Model
open Tokenizer
open Parser

let rec evalScript (actions: Actions) (variables: Variables) (script: Script) : Result<Variables * Any, LigatureError> =
    match script with
    | [] -> Ok(variables, Any.Network Set.empty)
    | head :: tail ->
        match head with
        | Assignment(variable, value) -> 
            failwith "TODO"
        | Application application ->
            match executeApplication actions variables application with
            | Ok(variables, _) -> evalScript actions variables tail
            | Error err -> Error err

and createAction (doc: string) (script: Script) examples pre post : Action =
    Action.Full(
        { doc = doc
          examples = examples
          pre = pre
          post = post },
          (fun actions variables -> evalScript actions variables script)
    )

and lookupAction (actions: Actions) (action: Term) : Action option =
    match Map.tryFind action actions with
    | Some(action) -> Some(action)
    | None -> None

and executeApplication (actions: Actions) (variables: Variables) (application: Any list) =
    match application with
    | [Any.Network network] -> 
        Ok(variables, network)
    | _ -> failwith "TODO"
    // match lookupAction actions action with
    // | Some(Action.Full(_, action)) -> action actions stack
    // | Some(Action.Stack(_, action)) ->
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
