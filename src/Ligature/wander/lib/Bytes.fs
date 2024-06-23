// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Bytes

open Ligature.Wander.Model
open Ligature.Main

let lengthFunction =
    { Name = "length"
      Eval =
        (fun args _ ->
            match args with
            | [ WanderValue.Bytes(bytes) ] -> Ok(WanderValue.Int(Array.length bytes |> bigint))
            | _ -> error "Invalid call to map function." None) }

let bytesLib = [ lengthFunction ]
