// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Bend.Lib.Array
open Ligature.Bend.Model
open Ligature
open FsToolkit.ErrorHandling
open Ligature.Bend.Interpreter

let distinctFunction<'t> = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args bindings ->
        match args with
        | [BendValue.Array(array)] -> Ok(BendValue.Array(Array.distinct array))
        | _ -> error "Improper call to map function." None))))

let catFunction<'t> = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args bindings ->
        match args with
        | [BendValue.Array(array)] -> 
            let res = Array.fold (fun state value ->
                match value with
                | BendValue.String(value) -> state + value
                | _ -> failwith "TODO") "" array
            Ok(BendValue.String(res))
        | _ -> error "Improper call to map function." None))))

let insertFirstFunction<'t> = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args bindings ->
        match args with
        | [value; BendValue.Array(array)] -> Ok(BendValue.Array(Array.insertAt 0 value array))
        | _ -> error "Improper call to Array.insertFirst function." None))))

let insertLastFunction<'t> = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args bindings ->
        match args with
        | [value; BendValue.Array(array)] -> Ok(BendValue.Array(Array.append [|value|] array))
        | _ -> error "Improper call to Array.insertLast function." None))))

let mapFunction<'t> = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args bindings ->
        match args with
        | [BendValue.Function(fn); BendValue.Array(array)] ->
            Array.map (fun value ->
                callFunction fn [value] bindings) array
            |> List.ofArray
            |> List.traverseResultM (fun x -> x)
            |> Result.map (fun x -> BendValue.Array (Array.ofList x))
        | _ -> error "Improper call to map function." None))))

let reduceFunction<'t> = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args bindings ->
        match args with
        | [BendValue.Function(fn); BendValue.Array(array)] ->
            Ok(Array.reduce (fun lValue rValue ->
                match callFunction fn [lValue; rValue] bindings with
                | Ok(res) -> res
                | Error(err) -> failwith (err.ToString())) array)
        | _ -> error "Improper call to Array.map function." None))))

let foldFunction<'t> = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args bindings ->
        match args with
        | [BendValue.Function(fn); initial; BendValue.Array(array)] ->
            Ok(Array.fold (fun state value ->
                match callFunction fn [state; value] bindings with
                | Ok(res) -> res
                | Error(err) -> failwith (err.ToString())) initial array)
        | _ -> error "Improper call to Array.fold function." None))))

let lengthFunction<'t> = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args _ ->
        match args with
        | [BendValue.Array(array)] -> Ok(BendValue.Int(Array.length array))
        | _ -> error "Improper call to map function." None))))

let arrayLib<'t> = BendValue.Record (Map [
    ("cat", catFunction)
    ("map", mapFunction)
    ("reduce", reduceFunction)
    ("fold", foldFunction)
    ("insertFirst", insertFirstFunction)
    ("insertLast", insertLastFunction)
    ("distinct", distinctFunction)
    ("length", lengthFunction)])
