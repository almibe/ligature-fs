// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Fns.Wander

open Wander.Model
open Ligature.Model

// let writeValueFunction =
//     { Name = "writeValue"
//       Returns = WanderType.String
//       Eval =
//         (fun args _ ->
//             match args with
//             | [ value ] -> Ok(Value.String(printAny value))
//             | value -> error $"Unexpected value - {value}." None) }

// let readValueFunction =
//     { Name = "readValue"
//       Returns = WanderType.Value
//       Eval =
//         (fun args _ ->
//             match args with
//             | [ Value.String(input) ] -> run input (newBindings ())
//             | value -> error $"Unexpected value - {value}." None) }

let wanderLib = [] // writeValueFunction; readValueFunction ]
