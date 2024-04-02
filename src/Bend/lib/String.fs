// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Bend.Lib.String
open Ligature.Bend.Model
open Ligature

let lengthFunction = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args _ ->
        match args with
        | [BendValue.String(value)] -> Ok(BendValue.Int(String.length value))
        | _ -> error "Invalid call to map function." None))))

let stringLib = BendValue.Record (Map [
    ("length", lengthFunction)])
