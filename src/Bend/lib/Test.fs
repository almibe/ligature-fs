// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Bend.Lib.Test
open Ligature.Bend.Model
open Ligature

let equalFun = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args _ ->
        match args with
        | [BendValue.String(desc); left: BendValue; right: BendValue] ->
            if left = right then
                Ok(BendValue.Nothing)
            else
                error $"{prettyPrint left} != {prettyPrint right}" None
        | _ -> error "Invalid call to equal function." None))))

let testLib = BendValue.Record (Map [
    ("equal", equalFun)
])
