// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Identifier

open Ligature.Wander.Model
open Ligature

let valueFunction<'t> =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(
                (fun args bindings ->
                    match args with
                    | [ WanderValue.Identifier(identifier) ] -> Ok(WanderValue.String(readIdentifier identifier))
                    | _ -> error "Invalid call to Statement.value function." None)
            )
        )
    )

let toBytesFunction<'t> =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.Identifier(value) ] ->
                        Ok(WanderValue.Bytes(System.Text.Encoding.UTF8.GetBytes(readIdentifier value)))
                    | _ -> error "Invalid call to map function." None)
            )
        )
    )

let fromBytesFunction<'t> =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.Bytes(bytes) ] ->
                        match identifier (System.Text.Encoding.UTF8.GetString(bytes)) with
                        | Ok(identifer) -> Ok(WanderValue.Identifier(identifer))
                        | Error(err) -> Error(err)
                    | _ -> error "Invalid call to map function." None)
            )
        )
    )

let identifierLib<'t> =
    WanderValue.Namespace(
        Map
            [ ("toBytes", toBytesFunction)
              ("fromBytes", fromBytesFunction)
              ("value", valueFunction) ]
    )
