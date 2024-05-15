// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Keylime

open Ligature.Wander.Model
open Ligature.Main
open System
open System.Collections.Generic

let createKeylimeNamespace (name: string) (store: Dictionary<string, Map<byte array, byte array>>) =
    WanderValue.Namespace(Map.ofList [
        ("add", WanderValue.Function(Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.Bytes(key); WanderValue.Bytes(value) ] -> 
                        let s = store.Item(name)
                        store.Remove(name)
                        store.Add(name, Map.add key value s)
                        Ok(WanderValue.Namespace(Map.empty))
                    | _ -> error "Unexpected values" None
                )
            )
        )))
        ("remove", WanderValue.Function(Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.Bytes(key) ] -> 
                        store.Remove(name)
                        store.Add(name, Map.remove key (store.Item(name)))
                        Ok(WanderValue.Namespace(Map.empty))
                    | _ -> error "Unexpected values" None
                )
            )
        )))
        ("get", WanderValue.Function(Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.Bytes(key) ] ->
                        // printfn "Key = %A" key
                        // printfn "Contains key = %A " (store.ContainsKey(key))
                        // printfn "Contains key = %A " (store.Keys.Contains(key))
                        // printfn "Keys = %A " (store.Keys)
                        
                        match Map.tryFind key (store.Item(name)) with
                        | Some value -> Ok(WanderValue.Bytes(value))
                        | _ -> Ok(WanderValue.Namespace(Map.empty))
                    | _ -> error "Unexpected values" None
                )
            )
        )))
        ("count", WanderValue.Function(Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ _ ] -> Ok(WanderValue.Int(store.Count))
                    | _ -> error "Unexpected values" None
                )
            )
        )))
        ("prefix", WanderValue.Function(Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.Bytes(prefix) ] ->
                        store.Item(name)
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

let openFunction (store: Dictionary<string, Map<byte array, byte array>>) =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.String(databaseName) ] ->
                        if store.ContainsKey databaseName then
                            Ok(createKeylimeNamespace databaseName store)
                        else
                            let newDb = Map.empty
                            store.Add(databaseName, newDb)
                            Ok(createKeylimeNamespace databaseName store)
                    | _ -> error "Invalid call to map function." None)
            )
        )
    )

let keylimeLib =
    let store = new Dictionary<string, Map<byte array, byte array>>()
    WanderValue.Namespace(
        Map [ 
            ("open", openFunction store) ])
