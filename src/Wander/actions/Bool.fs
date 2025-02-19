// // This Source Code Form is subject to the terms of the Mozilla Public
// // License, v. 2.0. If a copy of the MPL was not distributed with this
// // file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Fns.Bool

open Wander.Model
open Ligature.Model

// let notFunction =
//     { Eval =
//         (fun networks local modules variables args ->
//             match args with
//             | [ Any.Term(value) ] ->
//                 match value with
//                 | Term("true") -> Ok((Some(Any.Term(Term("false"))), networks, local, modules, variables))
//                 | Term("false") -> Ok((Some(Any.Term(Term("true"))), networks, local, modules, variables))
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
