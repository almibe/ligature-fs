// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Bool

open Ligature.Wander.Model
open Ligature.Main

let notFunction<'t> =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args.Head with
                    | WanderValue.Bool(value) -> Ok(WanderValue.Bool(not value))
                    | _ -> error "Invalid call to not function." None)
            )
        )
    )

let andFunction<'t> =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.Bool(left); WanderValue.Bool(right) ] -> Ok(WanderValue.Bool(left && right))
                    | _ -> error "Invalid call to and function." None)
            )
        )
    )

let toBytesFunction<'t> =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.Bool(false) ] -> Ok(WanderValue.Bytes([| 0uy |]))
                    | [ WanderValue.Bool(true) ] -> Ok(WanderValue.Bytes([| 1uy |]))
                    | _ -> error "Invalid call to Bool.toBytes function." None)
            )
        )
    )

let fromBytesFunction<'t> =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.Bytes([| 0uy |]) ] -> Ok(WanderValue.Bool(false))
                    | [ WanderValue.Bytes([| 1uy |]) ] -> Ok(WanderValue.Bool(true))
                    | _ -> error "Invalid call to Bool.fromBytes function." None)
            )
        )
    )

let boolLib<'t> =
    WanderValue.Namespace(
        Map
            [ ("toBytes", toBytesFunction)
              ("fromBytes", fromBytesFunction)
              ("not", notFunction)
              ("and", andFunction) ]
    )
