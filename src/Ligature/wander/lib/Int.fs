// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Int

open Ligature.Wander.Model
open Ligature.Main
open System

let ltFunction<'t> =
    new HostFunction(
        (fun args _ ->
            match args with
            | [ WanderValue.Int(left); WanderValue.Int(right) ] -> Ok(WanderValue.Bool(left < right))
            | _ -> error "Invalid call to map function." None)
    )

// let toBytesFunction<'t> =
//     WanderValue.Function(
//         Function.HostFunction(
//             new HostFunction(
//                 (fun args _ ->
//                     match args with
//                     | [ WanderValue.Int(value) ] -> Ok(WanderValue.Bytes(BitConverter.GetBytes value))
//                     | _ -> error "Invalid call to map function." None)
//             )
//         )
//     )

// let fromBytesFunction<'t> =
//     WanderValue.Function(
//         Function.HostFunction(
//             new HostFunction(
//                 (fun args _ ->
//                     match args with
//                     | [ WanderValue.Bytes(value) ] -> Ok(WanderValue.Int(BitConverter.ToInt64 value))
//                     | _ -> error "Invalid call to map function." None)
//             )
//         )
//     )

let intLib<'t> = Map [ ("lt", ltFunction) ]
//              ("toBytes", toBytesFunction)
//              ("fromBytes", fromBytesFunction) ]
