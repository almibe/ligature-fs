// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Ulid

open Ligature.Wander.Model
open Ligature.Main
open System

// let nextFunction =
// #if !FABLE_COMPILER
//     { Name = "next"
//       Returns = WanderType.Identifier
//       Eval =
//         (fun args _ ->
//             match args with
//             | [ WanderValue.String(prefix) ] ->
//                 match identifier (prefix + Ulid.NewUlid().ToString()) with
//                 | Ok identifier -> Ok(WanderValue.Identifier(identifier))
//                 | _ -> error $"Invalid prefix for Identifier {prefix}." None
//             | _ -> error "Invalid call to Ulid.next function." None) }
// #else
//     () //should never reach
// #endif

let ulidLib = [] // nextFunction ]
