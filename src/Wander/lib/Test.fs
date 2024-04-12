// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Test

open Ligature.Wander.Model
open Ligature

let equalFun<'t> =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.String(desc); left; right ] ->
                        if left = right then
                            Ok(WanderValue.Nothing)
                        else
                            error $"{prettyPrint left} != {prettyPrint right}" None
                    | _ -> error "Invalid call to equal function." None)
            )
        )
    )

let testLib<'t> = WanderValue.Record(Map [ ("equal", equalFun) ])
