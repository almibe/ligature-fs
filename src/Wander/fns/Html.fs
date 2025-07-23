// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Fns.Html

open Ligature.Model
open Wander.Model
open Feliz.ViewEngine

let printDataColumn (elements: Set<Element>) : string =
    match List.ofSeq elements with
    | [] -> ""
    | [ { value = Term single } ] -> single
    | multi ->
        List.fold
            (fun state { value = Term value } ->
                match state with
                | "" -> value
                | _ -> $"{state}, {value}")
            ""
            multi

let forceDirectedLayoutFn: Fn =
    Fn.Fn(
        { doc = "Generate a force directed layout for the given set of assertions."
          examples = [ "force-directed-layout(assertions(a {rel(b c)}))" ]
          args = "Assertions"
          result = "Node" },
        fun _ _ application ->
            match application.arguments with
            | [ Expression.Assertions assertions ] -> failwith "TODO"
            | _ -> failwith "Invalid call to force-directed-layout."
    )
