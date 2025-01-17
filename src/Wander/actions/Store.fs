// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Actions.Store

open Wander.Model
open Ligature.Model
open System
open System.Collections.Generic

let mergeAction =
    { Eval =
        (fun actions network stack ->
            match stack with
            | Any.Network network :: tail -> failwith "TODO"
            | _ -> error "Invalid call merge action." None) }

let removeAction =
    { Eval =
        (fun actions network stack ->
            match stack with
            | Any.Network network :: tail -> failwith "TODO"
            | _ -> error "Invalid call to remove action." None) }

let readAction = { Eval = (fun actions network stack -> failwith "TODO") }
