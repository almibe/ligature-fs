// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Commands.IO

open Ligature.Model
open Wander.Model
open Wander.Interpreter

let openLocalDependencyCommand: Command =
    { Name = Element("open-local-dependency")
      Doc = "Open a local dependency."
      Eval =
        fun commands variables arguments ->
            match arguments with
            | [ Any.Literal filePath ] ->
                let script = System.IO.File.ReadAllText(filePath)
                
                failwith "TODO"
            //Ok(Some(value))
            | _ -> failwith "id requires 1 argument." }

let ioCommands =
    (Map.ofList [ (openLocalDependencyCommand.Name, openLocalDependencyCommand) ])
