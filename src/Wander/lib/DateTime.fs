// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.DateTime

open Ligature.Wander.Model
open Ligature
open System

let ticksFunction<'t> =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ _ ] -> Ok(WanderValue.Int(DateTime.Now.Ticks))
                    | _ -> error "Invalid call to map function." None)
            )
        )
    )

let dateTimeLib<'t> = WanderValue.Record(Map [ ("ticks", ticksFunction) ])
