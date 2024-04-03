// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Bend.Lib.Bool
open Ligature.Bend.Model
open Ligature

let notFunction = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args _ ->
        match args.Head with
        | BendValue.Bool(value) -> Ok(BendValue.Bool(not value))
        | _ -> error "Invalid call to not function." None))))

let andFunction = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args _ ->
        match args with
        | [BendValue.Bool(left); BendValue.Bool(right)] -> Ok(BendValue.Bool(left && right))
        | _ -> error "Invalid call to and function." None))))

let toBytesFunction = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args _ ->
        match args with
        | [BendValue.Bool(false)] -> Ok(BendValue.Bytes([|0uy|]))
        | [BendValue.Bool(true)] -> Ok(BendValue.Bytes([|1uy|]))
        | _ -> error "Invalid call to Bool.toBytes function." None))))

let fromBytesFunction = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args _ ->
        match args with
        | [BendValue.Bytes([|0uy|])] -> Ok(BendValue.Bool(false))
        | [BendValue.Bytes([|1uy|])] -> Ok(BendValue.Bool(true))
        | _ -> error "Invalid call to Bool.fromBytes function." None))))

let boolLib = BendValue.Record (Map [
    ("toBytes", toBytesFunction)
    ("fromBytes", fromBytesFunction)
    ("not", notFunction)
    ("and", andFunction)])
