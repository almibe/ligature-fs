// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Actions.Store

open Wander.Model
open Ligature.Model
open System
open System.Collections.Generic

// let networksFunction (store: LigatureStore) =
//     { Module = "Store"
//       Name = "networks"
//       Description = "Get an Array of all of the Networks in this Store."
//       Parameters = []
//       Returns = WanderType.Array
//       Eval =
//         (fun args _ ->
//             match args with
//             | [ _ ] ->
//                 store.networks ()
//                 |> Seq.map Value.String
//                 |> Seq.toList
//                 |> Value.Quote
//                 |> Ok
//             | _ -> error "Invalid call to map function." None) }

// let addNetworkFunction (store: LigatureStore) =
//     { Module = "Store"
//       Name = "addNetwork"
//       Description = ""
//       Parameters = []
//       Returns = WanderType.Nothing
//       Eval =
//         (fun args _ ->
//             match args with
//             | [ Value.String(name) ] ->
//                 store.addNetwork name
//                 Ok(Value.Nothing)
//             | _ -> error "Invalid call to map function." None) }

// let removeNetworkFunction (store: LigatureStore) =
//     { Module = "Store"
//       Name = "removeNetwork"
//       Description = ""
//       Parameters = []
//       Returns = WanderType.Nothing
//       Eval =
//         (fun args _ ->
//             match args with
//             | [ Value.String(name) ] ->
//                 store.removeNetwork name
//                 Ok(Value.Nothing)
//             | _ -> error "Invalid call to map function." None) }

// let addFunction (store: LigatureStore) =
//     { Module = "Store"
//       Name = "add"
//       Description = ""
//       Parameters = []
//       Returns = WanderType.Nothing
//       Eval =
//         (fun args _ ->
//             match args with
//             | [ Value.String(name); Value.Network(network) ] ->
//                 store.add name network |> ignore
//                 Ok(Value.Nothing)
//             | _ -> error "Invalid call to map function." None) }

// let removeFunction (store: LigatureStore) =
//     { Module = "Store"
//       Name = "remove"
//       Description = ""
//       Parameters = []
//       Returns = WanderType.Nothing
//       Eval =
//         (fun args _ ->
//             match args with
//             | [ Value.String(name); Value.Network(network) ] ->
//                 store.remove name network |> ignore
//                 Ok(Value.Nothing)
//             | _ -> error "Invalid call to map function." None)

//     }

// let readFunction (store: LigatureStore) =
//     { Module = "Store"
//       Name = "read"
//       Description = ""
//       Parameters = []
//       Returns = WanderType.Network
//       Eval =
//         (fun args _ ->
//             match args with
//             | [ Value.String(name) ] -> Ok(Value.Network(store.read name))
//             | _ -> error "Invalid call to map function." None) }

let storeLib store = []
// [ (networksFunction store)
//   (addNetworkFunction store)
//   (removeNetworkFunction store)
//   (addFunction store)
//   (removeFunction store)
//   (readFunction store) ]
