// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Lib.Store

open Wander.Model
open Ligature.Main
open System
open System.Collections.Generic
open Ligature.LigatureStore

let networksFunction (store: LigatureStore) =
    { Module = "Store"
      Name = "networks"
      Description = "Get an Array of all of the Networks in this Store."
      Parameters = []
      Returns = WanderType.Array
      Eval =
        (fun args _ ->
            match args with
            | [ _ ] ->
                store.networks ()
                |> Seq.map WanderValue.String
                |> Seq.toList
                |> WanderValue.Quote
                |> Ok
            | _ -> error "Invalid call to map function." None) }

let addNetworkFunction (store: LigatureStore) =
    { Module = "Store"
      Name = "addNetwork"
      Description = ""
      Parameters = []
      Returns = WanderType.Nothing
      Eval =
        (fun args _ ->
            match args with
            | [ WanderValue.String(name) ] ->
                store.addNetwork name
                Ok(WanderValue.Nothing)
            | _ -> error "Invalid call to map function." None) }

let removeNetworkFunction (store: LigatureStore) =
    { Module = "Store"
      Name = "removeNetwork"
      Description = ""
      Parameters = []
      Returns = WanderType.Nothing
      Eval =
        (fun args _ ->
            match args with
            | [ WanderValue.String(name) ] ->
                store.removeNetwork name
                Ok(WanderValue.Nothing)
            | _ -> error "Invalid call to map function." None) }

let addFunction (store: LigatureStore) =
    { Module = "Store"
      Name = "add"
      Description = ""
      Parameters = []
      Returns = WanderType.Nothing
      Eval =
        (fun args _ ->
            match args with
            | [ WanderValue.String(name); WanderValue.Network(network) ] ->
                store.add name network |> ignore
                Ok(WanderValue.Nothing)
            | _ -> error "Invalid call to map function." None) }

let removeFunction (store: LigatureStore) =
    { Module = "Store"
      Name = "remove"
      Description = ""
      Parameters = []
      Returns = WanderType.Nothing
      Eval =
        (fun args _ ->
            match args with
            | [ WanderValue.String(name); WanderValue.Network(network) ] ->
                store.remove name network |> ignore
                Ok(WanderValue.Nothing)
            | _ -> error "Invalid call to map function." None)

    }

let readFunction (store: LigatureStore) =
    { Module = "Store"
      Name = "read"
      Description = ""
      Parameters = []
      Returns = WanderType.Network
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
