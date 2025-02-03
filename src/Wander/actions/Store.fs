// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Actions.Store

open Wander.Model
open Ligature.Model

let mergeAction =
    Action.Full(
        { doc = "Take a Network Name and Network and merge the Network into the Named Network."
          examples = [] },
        fun _ networks stack ->
            match stack with
            | Any.NetworkName name :: Any.Network network :: tail ->
                match Map.tryFind name networks with
                | Some(currenNetwork) -> Ok(Map.add name (Set.union network currenNetwork) networks, tail)
                | None -> Ok(Map.add name network networks, tail)
            | _ -> error "Invalid call merge action." None
    )

let removeAction =
    Action.Full(
        { doc = "..."; examples = [] },
        fun _ networks stack ->
            match stack with
            | Any.NetworkName name :: Any.Network network :: tail ->
                match Map.tryFind name networks with
                | Some(currenNetwork) -> Ok(Map.add name (Set.difference currenNetwork network) networks, tail)
                | None -> Ok(networks, tail)
            | _ -> error "Invalid call remove action." None
    )

let readAction =
    Action.Full(
        { doc = "Read a Network Name off the Stack and then push that Network's value onto the Stack."
          examples = [] },
        fun _ networks stack ->
            match stack with
            | Any.NetworkName name :: tail ->
                match Map.tryFind name networks with
                | Some(currenNetwork) -> Ok(networks, Any.Network currenNetwork :: tail)
                | None -> failwith "TODO"
            | _ -> error "Invalid call remove action." None
    )
