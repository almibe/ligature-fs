// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Interpreter

open Ligature.Model
open Model

let executeAction (actions: Actions) (networks: Networks) (stack: Stack) (action: Element) =
    match Map.tryFind action actions with
    | Some(action) -> action.Eval actions networks stack
    | None -> error $"Could not find action {action}." None

let rec evalScript
    (actions: Actions)
    (networks: Networks)
    (stack: Stack)
    (script: Script)
    : Result<Networks * Stack, LigatureError> =
    match script with
    | [] -> Ok(networks, stack)
    | head :: tail ->
        match head with
        | Any.Element action ->
            match executeAction actions networks stack action with
            | Ok(_, stack) -> evalScript actions networks stack tail
            | Error err -> Error err
        | value -> evalScript actions networks (value :: stack) tail
