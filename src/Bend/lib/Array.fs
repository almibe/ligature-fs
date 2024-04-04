// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Bend.Lib.Array
open Ligature.Bend.Model
open Ligature
open FsToolkit.ErrorHandling
open Ligature.Bend.Interpreter

let mapFunction = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args bindings ->
        match args with
        | [BendValue.Function(fn); BendValue.Array(array)] ->
            List.map (fun value ->
                callFunction fn [value] bindings) array
            |> List.traverseResultM (fun x -> x)
            |> Result.map (fun x -> BendValue.Array x)
        | _ -> error "Invalid call to map function." None))))

let reduceFunction = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args bindings ->
        match args with
        | [BendValue.Function(fn); BendValue.Array(array)] ->
            Ok(List.reduce (fun lValue rValue ->
                match callFunction fn [lValue; rValue] bindings with
                | Ok(res) -> res
                | Error(err) -> failwith (err.ToString())) array)
        | _ -> error "Invalid call to map function." None))))

let lengthFunction = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args _ ->
        match args with
        | [BendValue.Array(array)] -> Ok(BendValue.Int(List.length array))
        | _ -> error "Invalid call to map function." None))))

let arrayLib = BendValue.Record (Map [
    ("map", mapFunction)
    ("reduce", reduceFunction)
    ("length", lengthFunction)])
