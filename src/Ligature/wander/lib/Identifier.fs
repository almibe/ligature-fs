// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Name

open Ligature.Wander.Model
open Ligature.Main

// let valueFunction =
//     { Name = "value"
//       Returns = WanderType.Value
//       Eval =
//         (fun args bindings ->
//             match args with
//             | [ WanderValue.Name(identifier) ] -> Ok(WanderValue.String(readName identifier))
//             | _ -> error "Invalid call to Triple.value function." None) }

// let toBytesFunction =
//     { Name = "toBytes"
//       Returns = WanderType.Bytes
//       Eval =
//         (fun args _ ->
//             match args with
//             | [ WanderValue.Name(value) ] ->
//                 Ok(WanderValue.Bytes(System.Text.Encoding.UTF8.GetBytes(readName value)))
//             | _ -> error "Invalid call to map function." None) }

// let fromBytesFunction =
//     { Name = "fromBytes"
//       Returns = WanderType.Name
//       Eval =
//         (fun args _ ->
//             match args with
//             | [ WanderValue.Bytes(bytes) ] ->
//                 match identifier (System.Text.Encoding.UTF8.GetString(bytes)) with
//                 | Ok(identifer) -> Ok(WanderValue.Name(identifer))
//                 | Error(err) -> Error(err)
//             | _ -> error "Invalid call to map function." None) }

let identifierLib = [] // toBytesFunction; fromBytesFunction; valueFunction ]
