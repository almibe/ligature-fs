// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Commands.TinyDL

open Ligature.Model
open Wander.Model
open TinyDL.Model

let inferCommand: Command =
    { Name = Element("infer")
      Doc = "Infer all supported tiny-dl relations."
      Eval =
        fun _ variables arguments ->
            match arguments with
            | [ description; network ] ->
                let description =
                    match description with
                    | Any.Network n -> n
                    | Any.Variable v ->
                        match variables.TryFind v with
                        | Some(Any.Network n) -> n
                        | _ -> failwith "TODO"
                    | _ -> failwith "TODO"

                let network =
                    match network with
                    | Any.Network n -> n
                    | Any.Variable v ->
                        match variables.TryFind v with
                        | Some(Any.Network n) -> n
                        | _ -> failwith "TODO"
                    | _ -> failwith "TODO"

                match infer (networkToDescription description) network with
                | Ok res -> Ok(Some(Any.Network res))
                | Error err -> error $"Error calling infer: {err}" None
                | _ -> error "Unexpected return value from infer." None
            | _ -> error "Improper call to infer." None }

let tinyDLCommands = (Map.ofList [ (inferCommand.Name, inferCommand) ])
