// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Actions.IO

open Ligature.Model
open Wander.Model
open Wander.Main

// let openLocalDependencyCommand: Command =
//     { Eval =
//         fun networks local modules variables arguments ->
//             match arguments with
//             | [ Any.Literal filePath ] ->
//                 let script = System.IO.File.ReadAllText(filePath)

//                 match run networks local modules variables script with
//                 | Ok((_, networks, local, modules, variables)) -> Ok((None, networks, local, modules, variables))
//                 | _ -> failwith "TODO"
//             | _ -> failwith "open-local-dependency requires 1 argument." }

// let openLocalLibraryCommand: Command =
//     { Eval =
//         fun networks local modules variables arguments ->
//             match arguments with
//             | [ Any.Literal filePath ] ->
//                 let path = System.Environment.GetEnvironmentVariable("WANDER_LIBS")
//                 let script = System.IO.File.ReadAllText(path + "/" + filePath)

//                 match run networks local modules variables script with
//                 | Ok((_, networks, local, modules, variables)) -> Ok((None, networks, local, modules, variables))
//                 | _ -> failwith "TODO"
//             | _ -> failwith "open-local-library requires 1 argument." }

let ioCommands = (Map.empty)
// [ (Element "open-local-dependency", openLocalDependencyCommand)
//   (Element "open-local-library", openLocalLibraryCommand) ])
