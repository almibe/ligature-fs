// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Commands.TinyDL

open Ligature.Main
open TinyDL.Model
open Wander.Model

let inferCommand: Command =
    { Name = Symbol("infer")
      Doc = "Use the ."
      Signature = [ WanderType.Network; WanderType.Network ], None
      Eval =
        fun _ store arguments ->
            match arguments with
            | [ WanderValue.Network(description); WanderValue.Network(network) ] ->
                match infer (networkToDescription description) network with
                | Ok res -> Ok(Some(WanderValue.Network res))
                | Error err -> error $"Error calling infer: {err}" None
                | _ -> error "Unexpected return value from infer." None
            | _ -> error "Improper call to infer." None }

let parseCommand: Command =
    { Name = Symbol "tiny-dl.parse"
      Doc = "Parse tiny-dl script into a Network."
      Signature = [ WanderType.Symbol ], None
      Eval =
        fun _ store arguments ->
            match arguments with
            | [ WanderValue.Symbol(input) ] -> failwith "TODO" }

let tinyDLCommands = (Map.ofList [ (inferCommand.Name, inferCommand) ])
