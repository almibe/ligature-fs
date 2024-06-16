// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Store

open Ligature.Wander.Model
open Ligature.Main
open System
open System.Collections.Generic
open Ligature.LigatureStore

let networksFunction (store: LigatureStore) =
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

let addNetworkFunction (store: LigatureStore) =
    new HostFunction(
        (fun args _ ->
            match args with
            | [ WanderValue.String(name) ] ->
                store.addNetwork name
                Ok(WanderValue.Nothing)
            | _ -> error "Invalid call to map function." None)
    )

let removeNetworkFunction (store: LigatureStore) =
    new HostFunction(
        (fun args _ ->
            match args with
            | [ WanderValue.String(name) ] ->
                store.removeNetwork name
                Ok(WanderValue.Nothing)
            | _ -> error "Invalid call to map function." None)
    )

let addFunction (store: LigatureStore) =
    new HostFunction(
        (fun args _ ->
            match args with
            | [ WanderValue.String(name); WanderValue.Network(network) ] ->
                store.add name network |> ignore
                Ok(WanderValue.Nothing)
            | _ -> error "Invalid call to map function." None)
    )

let removeFunction (store: LigatureStore) =
    new HostFunction(
        (fun args _ ->
            match args with
            | [ WanderValue.String(name); WanderValue.Network(network) ] ->
                store.remove name network |> ignore
                Ok(WanderValue.Nothing)
            | _ -> error "Invalid call to map function." None)
    )

let readFunction (store: LigatureStore) =
    new HostFunction(
        (fun args _ ->
            match args with
            | [ WanderValue.String(name) ] -> Ok(WanderValue.Network(store.read name))
            | _ -> error "Invalid call to map function." None)
    )

let storeLib store =
    Map
        [ ("networks", networksFunction store)
          ("addNetwork", addNetworkFunction store)
          ("removeNetwork", removeNetworkFunction store)
          ("add", addFunction store)
          ("remove", removeFunction store)
          ("read", readFunction store) ]
