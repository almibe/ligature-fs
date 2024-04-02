// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Bend.Lib.Array
open Ligature.Bend.Model
open Ligature
open FsToolkit.ErrorHandling

let mapFunction = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args bindings ->
        match args with
        | [BendValue.Function(fn); BendValue.Array(array)] ->
            match fn with
            | Function.Lambda(_, _) -> failwith "TODO"
            | Function.HostFunction(fn) ->
                List.map (fun value ->
                    fn.Run [value] bindings) array
                |> List.traverseResultM (fun x -> x)
                |> Result.map (fun x -> BendValue.Array x)
        | _ -> error "Invalid call to map function." None))))

let lengthFunction = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args _ ->
        match args with
        | [BendValue.Array(array)] -> Ok(BendValue.Int(List.length array))
        | _ -> error "Invalid call to map function." None))))

let arrayLib = BendValue.Record (Map [
    ("map", mapFunction)
    ("length", lengthFunction)
])
