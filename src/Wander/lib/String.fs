// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.String

open Ligature.Wander.Model
open Ligature

let catFunction =
    WanderValue.Function(
        Function.HostFunction(
            HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.Array(values) ] -> 
                        Array.fold (fun state value -> 
                            match value with
                            | WanderValue.String(value) -> state + value
                            | _ -> failwith "Error") "" values
                        |> WanderValue.String
                        |> Ok
                    | _ -> error "Invalid call to map function." None)
            )
        )
    )

let lengthFunction =
    WanderValue.Function(
        Function.HostFunction(
            HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.String(value) ] -> Ok(WanderValue.Int(String.length value |> bigint))
                    | _ -> error "Invalid call to map function." None)
            )
        )
    )

let toBytesFunction =
    WanderValue.Function(
        Function.HostFunction(
            HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.String(value) ] -> Ok(WanderValue.Bytes(System.Text.Encoding.UTF8.GetBytes value))
                    | _ -> error "Invalid call to map function." None)
            )
        )
    )

let fromBytesFunction =
    WanderValue.Function(
        Function.HostFunction(
            HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.Bytes(bytes) ] -> Ok(WanderValue.String(System.Text.Encoding.UTF8.GetString bytes))
                    | _ -> error "Invalid call to map function." None)
            )
        )
    )

let stringLib =
    WanderValue.Namespace(
        Map
            [ ("cat", catFunction)
              ("fromBytes", fromBytesFunction)
              ("length", lengthFunction)
              ("toBytes", toBytesFunction) ]
    )
