// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Interpreter

open Ligature.Model
open Model

let rec evalScript (actions: Actions) (stack: Stack) (script: Script) : Result<Stack, LigatureError> =
    match script with
    | [] -> Ok(stack)
    | head :: tail ->
        match head with
        | Any.Element action ->
            match executeAction actions stack action with
            | Ok(stack) -> evalScript actions stack tail
            | Error err -> Error err
        | value -> evalScript actions (value :: stack) tail

and createAction (doc: string) (quote: Quote) examples pre post : Action =
    Action.Full(
        { doc = doc
          examples = examples
          pre = pre
          post = post },
        (fun actions stack -> evalScript actions stack quote)
    )

and lookupAction (actions: Actions) (action: Element) : Action option =
    match Map.tryFind action actions with
    | Some(action) -> Some(action)
    | None -> None

and executeAction (actions: Actions) (stack: Stack) (action: Element) =
    match lookupAction actions action with
    | Some(Action.Full(_, action)) -> action actions stack
    | Some(Action.Stack(_, action)) ->
        match action stack with
        | Ok stack -> Ok(stack)
        | Error err -> Error err
    | None -> error $"Could not find action {action}." None
