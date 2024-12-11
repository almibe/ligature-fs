// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Commands.TinyDL

open Ligature.Main
//open TinyDL.Model
open Wander.Model

// let inferCommand: Command =
//     { Name = Element("infer")
//       Doc = "Use the ."
//       Eval =
//         fun _ store arguments ->
//             match arguments with
//             | [ Value.Network(description); Value.Network(network) ] ->
//                 match infer (networkToDescription description) network with
//                 | Ok res -> Ok(Some(Value.Network res))
//                 | Error err -> error $"Error calling infer: {err}" None
//                 | _ -> error "Unexpected return value from infer." None
//             | _ -> error "Improper call to infer." None }

let parseCommand: Command =
    { Name = Element "tiny-dl.parse"
      Doc = "Parse tiny-dl script into a Network."
      Eval =
        fun _ store arguments ->
            match arguments with
            | [ Value.Literal(input) ] -> failwith "TODO" }

// let tinyDLCommands = (Map.ofList [
//   // (inferCommand.Name, inferCommand)
//   ])
