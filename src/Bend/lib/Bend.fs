// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Bend.Lib.Bend

open Ligature.Bend.Model
open Ligature
open Ligature.Bend.Main
open Ligature.Bend.Bindings

let writeValueFunction<'t> = BendValue.Function(Function.HostFunction (
    new HostFunction(fun args _ ->
        match args with
        | [value] -> Ok(BendValue.String(prettyPrint value))
        | value   -> error $"Unexpected value - {value}." None)))

let readValueFunction<'t> = BendValue.Function(Function.HostFunction (
    new HostFunction(fun args _ ->
        match args with
        | [BendValue.String(input)] -> run input (newBindings ())
        | value -> error $"Unexpected value - {value}." None)))

let bendLib<'t> = BendValue.Record (Map [
    ("writeValue", writeValueFunction)
    ("readValue", readValueFunction)])
