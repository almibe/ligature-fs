// // This Source Code Form is subject to the terms of the Mozilla Public
// // License, v. 2.0. If a copy of the MPL was not distributed with this
// // file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Commands.Bool

open Wander.Model
open Ligature.Model

// let notFunction =
//     { Eval =
//         (fun networks local modules variables args ->
//             match args with
//             | [ Any.Element(value) ] ->
//                 match value with
//                 | Element("true") -> Ok((Some(Any.Element(Element("false"))), networks, local, modules, variables))
//                 | Element("false") -> Ok((Some(Any.Element(Element("true"))), networks, local, modules, variables))
//                 | _ -> error "Invalid argument passed to not." None
//             | _ -> error "Invalid call to not function." None) }

// let andFunction =
//     { Name = "and"
//       Returns = WanderType.Name
//       Eval =
//         (fun args _ ->
//             match args with
//             | [ Value.Bool(left); Value.Bool(right) ] -> Ok(Value.Bool(left && right))
//             | _ -> error "Invalid call to and function." None) }

let boolLib = Map.empty//.ofList [ ]//(Element "not", notFunction) ] // toBytesFunction; fromBytesFunction; notFunction; andFunction ]
