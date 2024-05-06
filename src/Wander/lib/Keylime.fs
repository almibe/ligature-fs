// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Keylime

open Ligature.Wander.Model
open Ligature
open System

let rec createKeylimeNamespace (store: Map<byte array, byte array>) =
    WanderValue.Namespace(Map.ofList [
        ("add", WanderValue.Function(Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.Bytes(key); WanderValue.Bytes(value) ] -> Ok (createKeylimeNamespace (Map.add key value store))
                    | _ -> error "Unexpected values" None
                )
            )
        )))
        ("remove", WanderValue.Function(Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.Bytes(key) ] -> Ok (createKeylimeNamespace (Map.remove key store))
                    | _ -> error "Unexpected values" None
                )
            )
        )))
        ("get", WanderValue.Function(Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.Bytes(key) ] ->
                        match Map.tryFind key store with
                        | Some value -> Ok(WanderValue.Bytes(value))
                        | None -> Ok(WanderValue.Namespace(Map.empty))
                    | _ -> error "Unexpected values" None
                )
            )
        )))
        ("all", WanderValue.Function(Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ _ ] ->
                        store
                        |> Map.toArray
                        |> Array.map (fun (k, v) -> WanderValue.Array([| WanderValue.Bytes(k); WanderValue.Bytes(v) |]))
                        |> WanderValue.Array
                        |> Ok
                    | _ -> error "Unexpected values" None
                )
            )
        )))
        ("prefix", WanderValue.Function(Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.Bytes(prefix) ] ->
                        store
                        |> Map.toArray
                        |> Array.filter (fun (key, _) -> (Array.truncate (Array.length prefix) key) = prefix )
                        |> Array.map (fun (k, v) -> WanderValue.Array [|WanderValue.Bytes(k); WanderValue.Bytes(v)|])
                        |> WanderValue.Array
                        |> Ok
                    | _ -> error "Unexpected values" None
                )
            )
        )))
    ])

let newFunction =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.Array(values) ] -> 
                        Array.fold (fun state value -> 
                            match value with
                            | WanderValue.Array([|WanderValue.Bytes(k); WanderValue.Bytes(v)|]) -> Map.add k v state
                            | _ -> failwith "Error") Map.empty values
                        |> createKeylimeNamespace
                        |> Ok
                    | _ -> error "Invalid call to map function." None)
            )
        )
    )

let keylimeLib =
    WanderValue.Namespace(
        Map [ 
            ("new", newFunction) ])

