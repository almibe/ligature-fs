// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Commands.IO

open Ligature.Model
open Wander.Model
open Wander.Main

let openLocalDependencyCommand: Command =
    { Name = Element("open-local-dependency")
      Doc = "Open a local dependency."
      Eval =
        fun local modules variables arguments ->
            match arguments with
            | [ Any.Literal filePath ] ->
                let script = System.IO.File.ReadAllText(filePath)

                match run local modules variables script with
                | Ok((_, local, modules, variables)) -> Ok((None, local, modules, variables))
                | _ -> failwith "TODO"
            //Ok(Some(value))
            | _ -> failwith "open-local-dependency requires 1 argument." }

let openLocalLibraryCommand: Command =
    { Name = Element("open-local-library")
      Doc = "Open a local dependency that is in the $WANDER_LIBS directory."
      Eval =
        fun local modules variables arguments ->
            match arguments with
            | [ Any.Literal filePath ] ->
                let path = System.Environment.GetEnvironmentVariable("WANDER_LIBS")
                let script = System.IO.File.ReadAllText(path + "/" + filePath)

                match run local modules variables script with
                | Ok((_, local, modules, variables)) -> Ok((None, local, modules, variables))
                | _ -> failwith "TODO"
            //Ok(Some(value))
            | _ -> failwith "open-local-library requires 1 argument." }

let ioCommands =
    (Map.ofList
        [ (openLocalDependencyCommand.Name, openLocalDependencyCommand)
          (openLocalLibraryCommand.Name, openLocalLibraryCommand) ])
