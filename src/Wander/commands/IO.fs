// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Commands.IO

open Ligature.Model
open Wander.Model
open Wander.Main

let openLocalDependencyCommand: Command =
    { Eval =
        fun local modules variables arguments ->
            match arguments with
            | [ Any.Literal filePath ] ->
#if !FABLE_COMPILER
                let script = System.IO.File.ReadAllText(filePath)
#else
                let script = failwith "TODO"
#endif
                match run local modules variables script with
                | Ok((_, local, modules, variables)) -> Ok((None, local, modules, variables))
                | _ -> failwith "TODO"
            | _ -> failwith "open-local-dependency requires 1 argument." }

let openLocalLibraryCommand: Command =
    { Eval =
        fun local modules variables arguments ->
            match arguments with
            | [ Any.Literal filePath ] ->
#if !FABLE_COMPILER
                let path = System.Environment.GetEnvironmentVariable("WANDER_LIBS")
                let script = System.IO.File.ReadAllText(path + "/" + filePath)
#else
                let path = failwith "TODO"
                let script = failwith "TODO"
#endif

                match run local modules variables script with
                | Ok((_, local, modules, variables)) -> Ok((None, local, modules, variables))
                | _ -> failwith "TODO"
            | _ -> failwith "open-local-library requires 1 argument." }

let ioCommands =
    (Map.ofList
        [ (Element "open-local-dependency", openLocalDependencyCommand)
          (Element "open-local-library", openLocalLibraryCommand) ])
