// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Wander

open Ligature.Wander.Model
open Ligature
open Ligature.Wander.Main
open Ligature.Wander.Bindings

let writeValueFunction<'t> = WanderValue.Function(Function.HostFunction (
    new HostFunction(fun args _ ->
        match args with
        | [value] -> Ok(WanderValue.String(prettyPrint value))
        | value   -> error $"Unexpected value - {value}." None)))

let readValueFunction<'t> = WanderValue.Function(Function.HostFunction (
    new HostFunction(fun args _ ->
        match args with
        | [WanderValue.String(input)] -> run input (newBindings ())
        | value -> error $"Unexpected value - {value}." None)))

let wanderLib<'t> = WanderValue.Record (Map [
    ("writeValue", writeValueFunction)
    ("readValue", readValueFunction)])
