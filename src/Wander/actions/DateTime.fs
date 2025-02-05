// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Actions.DateTime

open Wander.Model
open Ligature.Model
open System

// let ticksFunction =
//     { Name = "ticks"
//       Returns = WanderType.Int
//       Eval =
//         (fun args _ ->
//             match args with
//             | [ _ ] -> Ok(Value.Int(bigint DateTime.Now.Ticks))
//             | _ -> error "Invalid call to map function." None) }
