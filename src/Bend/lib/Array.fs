// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Bend.Lib.Array
open Ligature.Bend.Model
open Ligature.Bend
open Ligature

let mapFunction = Model.BendValue.HostFunction (
    new Model.HostFunction((fun args _ ->
        match args with
        | [Model.BendValue.(value), Model.BendValue] -> 
            Ok(Model.BendValue.Bool(not value))
        | _ -> error "Invalid call to map function." None)))

let arrayLib = BendValue.Record (Map [
    ("map", mapFunction)
])
