// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Interpreter

open Ligature.Model
open Model
open Tokenizer
open Parser

let rec evalScript (actions: Actions) (stack: Variables) (script: Script) : Result<Variables, LigatureError> =
    failwith "TODO"
    // match script with
    // | [] -> Ok(stack)
    // | head :: tail ->
    //     match head with
    //     | Any.Term action ->
    //         match executeAction actions stack action with
    //         | Ok(stack) -> evalScript actions stack tail
    //         | Error err -> Error err
    //     | value -> evalScript actions (value :: stack) tail

and createAction (doc: string) (quote: Quote) examples pre post : Action =
    failwith "TODO"
    // Action.Full(
    //     { doc = doc
    //       examples = examples
    //       pre = pre
    //       post = post },
    //     (fun actions stack -> evalScript actions stack quote)
    // )

and lookupAction (actions: Actions) (action: Term) : Action option =
    match Map.tryFind action actions with
    | Some(action) -> Some(action)
    | None -> None

and executeAction (actions: Actions) (stack: Variables) (action: Term) =
    match lookupAction actions action with
    | Some(Action.Full(_, action)) -> action actions stack
    | Some(Action.Stack(_, action)) ->
        match action stack with
        | Ok stack -> Ok(stack)
        | Error err -> Error err
    | None -> error $"Could not find action {action}." None

let read (input: string) : Result<Script, LigatureError> =
    try
        match tokenize input with
        | Ok tokens -> parse tokens
        | Error _ -> error "Error tokenizing." None
    with x ->
        error $"Error reading {x}" None
