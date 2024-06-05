// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.LigatureStore

open Ligature.Wander.Model
open Ligature.Main
open System
open System.Collections.Generic
open LightningDB

module LigatureLMDB =
    type LigatureLMDB(env: LightningEnvironment) =
        interface LigatureStore with
            member this.addNetwork networkName = failwith "TODO"
            member this.removeNetwork networkName = failwith "TODO"
            member this.networks() = failwith "TODO"
            member this.add name network = failwith "TODO"
            member this.remove name network = failwith "TODO"
            member this.query name network = failwith "TODO"

let createKeylimeNamespace () =
    let store = new Dictionary<byte array, byte array>()

    WanderValue.Namespace(
        Map.ofList
            [ ("add",
               WanderValue.Function(
                   Function.HostFunction(
                       new HostFunction(
                           (fun args _ ->
                               match args with
                               | [ WanderValue.Bytes(key); WanderValue.Bytes(value) ] ->
                                   store.Add(key, value)
                                   Ok(WanderValue.Namespace(Map.empty))
                               | _ -> error "Unexpected values" None)
                       )
                   )
               ))
              ("remove",
               WanderValue.Function(
                   Function.HostFunction(
                       new HostFunction(
                           (fun args _ ->
                               match args with
                               | [ WanderValue.Bytes(key) ] ->
                                   store.Remove(key)
                                   Ok(WanderValue.Namespace(Map.empty))
                               | _ -> error "Unexpected values" None)
                       )
                   )
               ))
              ("get",
               WanderValue.Function(
                   Function.HostFunction(
                       new HostFunction(
                           (fun args _ ->
                               match args with
                               | [ WanderValue.Bytes(key) ] ->
                                   match store.TryGetValue key with
                                   | (true, value) -> Ok(WanderValue.Bytes(value))
                                   | _ -> Ok(WanderValue.Namespace(Map.empty))
                               | _ -> error "Unexpected values" None)
                       )
                   )
               ))
              ("count",
               WanderValue.Function(
                   Function.HostFunction(
                       new HostFunction(
                           (fun args _ ->
                               match args with
                               | [ _ ] -> Ok(WanderValue.Int(store.Count))
                               | _ -> error "Unexpected values" None)
                       )
                   )
               ))
              ("prefix",
               WanderValue.Function(
                   Function.HostFunction(
                       new HostFunction(
                           (fun args _ ->
                               match args with
                               | [ WanderValue.Bytes(prefix) ] -> failwith "TODO"
                               // store.Item(name)
                               // |> Map.toArray
                               // |> Array.filter (fun (key, _) -> (Array.truncate (Array.length prefix) key) = prefix )
                               // |> Array.map (fun (k, v) -> WanderValue.Array [|WanderValue.Bytes(k); WanderValue.Bytes(v)|])
                               // |> WanderValue.Array
                               // |> Ok
                               | _ -> error "Unexpected values" None)
                       )
                   )
               )) ]
    )

let tempFunction =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ _ ] -> Ok(createKeylimeNamespace ())
                    | _ -> error "Invalid call to map function." None)
            )
        )
    )

let networksFunction (store: LightningEnvironment) =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ _ ] -> failwith "TODO"
                    // store.Keys
                    // |> Seq.map (fun storeName -> WanderValue.String storeName)
                    // |> Seq.toArray
                    // |> WanderValue.Array
                    // |> Ok
                    | _ -> error "Invalid call to map function." None)
            )
        )
    )

let inMemoryLib =
    let instance = InMemoryStore.empty ()

    WanderValue.Namespace(
        Map
            [ ("networks", networksFunction instance)
              ("addNetwork", addNetworkFunction instance)
              ("removeNetwork", removeNetworkFunction instance)
              ("add", addFunction instance)
              ("remove", removeFunction instance)
              ("read", queryFunction instance) ]
    )
