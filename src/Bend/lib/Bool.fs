// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Bend.Lib.Bool
open Ligature.Bend.Model

module Boolean =
    open Ligature.Bend
    open Ligature.Bend.Error
    let notFunction = Model.BendValue.HostFunction (
        new Model.HostFunction((fun args _ ->
            match args.Head with
            | Model.BendValue.Bool(value) -> Ok(Model.BendValue.Bool(not value))
            | _ -> error "Invalid call to not function." None)))

    let andFunction = Model.BendValue.HostFunction (
        new Model.HostFunction((fun args _ ->
            match args.Head with
            | Model.BendValue.Bool(value) -> Ok(Model.BendValue.Bool(not value))
            | _ -> error "Invalid call to and function." None)))

let boolLib = BendValue.Record (Map [
    ("not", Boolean.notFunction);])
