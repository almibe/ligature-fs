// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Actions.Store

open Wander.Model
open Ligature.Model

// let mergeAction =
//     Action.Full(
//         { doc = "Take a Network Name and Network and merge the Network into the Named Network."
//           examples = []; pre = ""; post = "" },
//         fun _ networks stack ->
//             match stack with
//             | Any.NetworkName name :: Any.Network network :: tail ->
//                 match Map.tryFind name networks with
//                 | Some(currenNetwork) -> Ok(Map.add name (Set.union network currenNetwork) networks, tail)
//                 | None -> Ok(Map.add name network networks, tail)
//             | _ -> error "Invalid call merge action." None
//     )

// let removeAction =
//     Action.Full(
//         { doc = "..."; examples = []; pre = ""; post = "" },
//         fun _ stack ->
//             match stack with
//             | Any.NetworkName name :: Any.Network network :: tail ->
//                 match Map.tryFind name networks with
//                 | Some(currenNetwork) -> Ok(Map.add name (Set.difference currenNetwork network) networks, tail)
//                 | None -> Ok(networks, tail)
//             | _ -> error "Invalid call remove action." None
//     )

let readAction =
    Action.Full(
        { doc = "Read a Network Name off the Stack and then push that Network's value onto the Stack."
          examples = []; pre = "NetworkName"; post = "Network" },
        fun _ stack -> failwith "TODO"
            // match stack with
            // | Any.NetworkName name :: tail ->
            //     match Map.tryFind name with
            //     | Some(currenNetwork) -> Ok(Any.Network currenNetwork :: tail)
            //     | None -> failwith "TODO"
            // | _ -> error "Invalid call remove action." None
    )
