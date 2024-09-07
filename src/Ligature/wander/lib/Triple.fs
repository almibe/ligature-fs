// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Triple

open Ligature.Wander.Model
open Ligature.Main
open FsToolkit.ErrorHandling

let slotIdentiferToWanderValue si =
    match si with
    | Pattern.Sl slot -> WanderValue.Slot(slot)
    | Pattern.Id identifier -> WanderValue.Name identifier

// let entityFunction =
//     { Name = "entity"
//       Returns = WanderType.Name
//       Eval =
//         (fun args bindings ->
//             match args with
//             | [ WanderValue.Triple(triple) ] -> Ok(slotIdentiferToWanderValue triple.Entity)
//             | _ -> error "Invalid call to Triple.entity function." None) }

// let attributeFunction =
//     { Name = "attribute"
//       Returns = WanderType.Name
//       Eval =
//         (fun args bindings ->
//             match args with
//             | [ WanderValue.Triple(triple) ] -> Ok(slotIdentiferToWanderValue triple.Attribute)
//             | _ -> error "Invalid call to Triple.attribute function." None) }

// let valueFunction =
//     { Name = "value"
//       Returns = WanderType.Name
//       Eval =
//         (fun args bindings ->
//             match args with
//             | [ WanderValue.Triple(triple) ] ->
//                 match triple.Value with
//                 | Value.Name(value) -> Ok(WanderValue.Name(value))
//                 | Value.String(value) -> Ok(WanderValue.String(value))
//                 | Value.Int(value) -> Ok(WanderValue.Int(value))
//                 | Value.Bytes(value) -> Ok(WanderValue.Bytes(value))
//                 | Value.Slot(value) -> failwith "TODO"
//             | _ -> error "Invalid call to Triple.value function." None) }

let tripleLib = [] // entityFunction; attributeFunction; valueFunction ]
