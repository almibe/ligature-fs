// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Interpreter

open Ligature.Model
open Model

let executeAction actions network stack action =

    failwith "TODO"

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
        | Any.Element action -> executeAction actions network stack action
        | value -> Ok(network, (value :: stack))
