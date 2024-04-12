// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Bend.Lib.Bytes

open Ligature.Bend.Model
open Ligature

let lengthFunction<'t> = BendValue.Function(Function.HostFunction (
    new HostFunction(fun args _ ->
        match args with
        | [BendValue.Bytes(bytes)] -> Ok(BendValue.Int(Array.length bytes))
        | _ -> error "Invalid call to map function." None)))

let bytesLib<'t> = BendValue.Record (Map [
    ("length", lengthFunction)])
