// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Bend.Lib.DateTime

open Ligature.Bend.Model
open Ligature
open System

let ticksFunction<'t> = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args _ ->
        match args with
        | [_] -> Ok(BendValue.Int(DateTime.Now.Ticks))
        | _ -> error "Invalid call to map function." None))))

let dateTimeLib<'t> = BendValue.Record (Map [
    ("ticks", ticksFunction)])
