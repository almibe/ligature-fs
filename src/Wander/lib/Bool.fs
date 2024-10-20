// // This Source Code Form is subject to the terms of the Mozilla Public
// // License, v. 2.0. If a copy of the MPL was not distributed with this
// // file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Lib.Bool

open Wander.Model
open Ligature.Main

let notFunction =
    { Name = Symbol("Bool.not")
      Doc = "Boolean not"
      Signature = [ LigatureType.Symbol ], Some(LigatureType.Symbol)
      Eval =
        (fun _ _ args ->
            match args with
            | [ WanderValue.Symbol(value) ] ->
                match value with
                | Symbol("true") -> Ok(Some(WanderValue.Symbol(Symbol("false"))))
                | Symbol("false") -> Ok(Some(WanderValue.Symbol(Symbol("true"))))
                | _ -> error "Invalid argument passed to not." None
            | _ -> error "Invalid call to not function." None) }

// let andFunction =
//     { Name = "and"
//       Returns = WanderType.Name
//       Eval =
//         (fun args _ ->
//             match args with
//             | [ WanderValue.Bool(left); WanderValue.Bool(right) ] -> Ok(WanderValue.Bool(left && right))
//             | _ -> error "Invalid call to and function." None) }

let boolLib = Map.ofList [ (notFunction.Name, notFunction) ] // toBytesFunction; fromBytesFunction; notFunction; andFunction ]
