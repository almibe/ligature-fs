// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Bytes

open Ligature.Wander.Model
open Ligature

let lengthFunction<'t> =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(fun args _ ->
                match args with
                | [ WanderValue.Bytes(bytes) ] -> Ok(WanderValue.Int(Array.length bytes))
                | _ -> error "Invalid call to map function." None)
        )
    )

let bytesLib<'t> = WanderValue.Record(Map [ ("length", lengthFunction) ])
