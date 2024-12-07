// // This Source Code Form is subject to the terms of the Mozilla Public
// // License, v. 2.0. If a copy of the MPL was not distributed with this
// // file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Commands.Bool

open Wander.Model
open Ligature.Main

let notFunction =
    { Name = Element("Bool.not")
      Doc = "Boolean not"
      Eval =
        (fun _ _ args ->
            match args with
            | [ WanderValue.Element(value) ] ->
                match value with
                | Element("true") -> Ok(Some(WanderValue.Element(Element("false"))))
                | Element("false") -> Ok(Some(WanderValue.Element(Element("true"))))
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
