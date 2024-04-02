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

let boolLib = BendValue.Record (Map [
    ("not", notFunction)
    ("and", andFunction)])
