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
    { Name = "networks"
      Eval =
        (fun args _ ->
            match args with
            | [ _ ] ->
                store.networks ()
                |> Seq.map WanderValue.String
                |> Seq.toArray
                |> WanderValue.Array
                |> Ok
            | _ -> error "Invalid call to map function." None) }

let addNetworkFunction (store: LigatureStore) =
    { Name = "addNetwork"
      Eval =
        (fun args _ ->
            match args with
            | [ WanderValue.String(name) ] ->
                store.addNetwork name
                Ok(WanderValue.Nothing)
            | _ -> error "Invalid call to map function." None) }

let removeNetworkFunction (store: LigatureStore) =
    { Name = "removeNetwork"
      Eval =
        (fun args _ ->
            match args with
            | [ WanderValue.String(name) ] ->
                store.removeNetwork name
                Ok(WanderValue.Nothing)
            | _ -> error "Invalid call to map function." None) }

let addFunction (store: LigatureStore) =
    { Name = "add"
      Eval =
        (fun args _ ->
            match args with
            | [ WanderValue.String(name); WanderValue.Network(network) ] ->
                store.add name network |> ignore
                Ok(WanderValue.Nothing)
            | _ -> error "Invalid call to map function." None) }

let removeFunction (store: LigatureStore) =
    { Name = "remove"
      Eval =
        (fun args _ ->
            match args with
            | [ WanderValue.String(name); WanderValue.Network(network) ] ->
                store.remove name network |> ignore
                Ok(WanderValue.Nothing)
            | _ -> error "Invalid call to map function." None)

    }

let readFunction (store: LigatureStore) =
    { Name = "read"
      Eval =
        (fun args _ ->
            match args with
            | [ WanderValue.String(name) ] -> Ok(WanderValue.Network(store.read name))
            | _ -> error "Invalid call to map function." None) }

let storeLib store =
    [ (networksFunction store)
      (addNetworkFunction store)
      (removeNetworkFunction store)
      (addFunction store)
      (removeFunction store)
      (readFunction store) ]
