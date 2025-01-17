// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Interpreter

open Ligature.Model
open Model

let executeAction (actions: Actions) (network: Network) (stack: Stack) (action: Element) =
    match Map.tryFind action actions with
    | Some(action) -> action.Eval actions network stack
    | None -> error $"Could not find action {action}." None

let rec evalScript
    (actions: Actions)
    (network: Network)
    (stack: Stack)
    (script: Script)
    : Result<Network * Stack, LigatureError> =
    match script with
    | [] -> Ok(network, stack)
    | head :: tail ->
        match head with
        | Any.Element action -> 
            match executeAction actions network stack action with
            | Ok(network, stack) -> evalScript actions network stack tail
            | Error err -> Error err
        | value -> evalScript actions network (value :: stack) tail
