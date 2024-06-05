// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.LigatureStore

open Ligature.Wander.Model
open Ligature.Main
open System
open System.Collections.Generic
open Pattern

type LigatureStore =
    abstract member networks : unit -> string seq
    abstract member addNetwork : string -> unit
    abstract member removeNetwork : string -> unit
    abstract member add : string -> Statement seq -> Result<unit, LigatureError>
    abstract member remove : string -> Statement seq -> Result<unit, LigatureError>
    abstract member read : string -> Statement seq

module InMemoryStore =
    type InMemoryStore(store: Dictionary<string, HashSet<Statement>>) =
        interface LigatureStore with
            member this.addNetwork networkName = 
                store.Add(networkName, new HashSet<Statement>())
            member this.removeNetwork networkName = 
                store.Remove(networkName) |> ignore
            member this.networks () = store.Keys
            member this.add name network =
                let store = store.Item(name)
                store.UnionWith(network)
                Ok(())
            member this.remove name network =
                let store = store.Item(name)
                store.ExceptWith(network)
                Ok(())
            member this.read name =
                let store = store.Item(name)
                store
    let empty (): LigatureStore = InMemoryStore(new Dictionary<string, HashSet<Statement>>())

let networksFunction (store: LigatureStore) =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ _ ] ->
                        store.networks ()
                        |> Seq.map (fun networkName -> WanderValue.String networkName)
                        |> Seq.toArray
                        |> WanderValue.Array
                        |> Ok
                    | _ -> error "Invalid call to map function." None)
            )
        )
    )

let addNetworkFunction (store: LigatureStore) =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.String(name) ] ->
                        store.addNetwork name
                        Ok(WanderValue.Nothing)
                    | _ -> error "Invalid call to map function." None)
            )
        )
    )

let removeNetworkFunction (store: LigatureStore) =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.String(name) ] ->
                        store.removeNetwork name
                        Ok(WanderValue.Nothing)
                    | _ -> error "Invalid call to map function." None)
            )
        )
    )

let addFunction (store: LigatureStore) =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.String(name); WanderValue.Pattern(pattern) ] ->
                        store.add name ((patternToNetwork pattern).all())
                        Ok(WanderValue.Nothing)
                    | _ -> error "Invalid call to map function." None)
            )
        )
    )

let removeFunction (store: LigatureStore) =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.String(name); WanderValue.Pattern(pattern) ] ->
                        store.remove name ((patternToNetwork pattern).all())
                        Ok(WanderValue.Nothing)
                    | _ -> error "Invalid call to map function." None)
            )
        )
    )

let readFunction (store: LigatureStore) =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.String(name) ] ->
                        store.read name
                        failwith "TODO"
                    | _ -> error "Invalid call to map function." None)
            )
        )
    )

let inMemoryLib =
    let store = InMemoryStore.empty ()
    WanderValue.Namespace(Map.ofList [
        ("networks", networksFunction store)
        ("addNetwork", addNetworkFunction store)
        ("removeNetwork", removeNetworkFunction store)
        ("add", addFunction store)
        ("remove", removeFunction store)
        ("read", readFunction store)
    ])
