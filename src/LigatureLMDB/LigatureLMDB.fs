// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Lmdb

open Model
open Wander.Main
open Wander.Library
open Wander.Model
open LightningDB

type IStore =
    abstract Networks: unit -> string seq
    abstract AddNetwork: string -> unit
    abstract RemoveNetwork: string -> unit
    abstract Merge: string -> Network -> unit
    abstract Remove: string -> Network -> unit
    abstract Query: string -> Network -> Network -> Network seq


type InMemoryStore(store: Ref<Map<string, Network>>) =

    interface IStore with
        member _.AddNetwork(name: string) : unit =
            store.Value <- Map.add name Set.empty store.contents

        member this.Merge (name: string) (network: Network) : unit =
            match store.contents.TryFind name with
            | Some currentNetwork -> store.Value <- Map.add name (Set.union currentNetwork network) store.contents
            | _ -> failwith "Not Implemented"

        member this.Networks() : string seq = store.contents.Keys

        member this.Query (name: string) (pattern: Network) (template: Network) : Network seq =
            match store.contents.TryFind name with
            | Some currentNetwork -> Ligature.Core.query pattern template currentNetwork
            | _ -> failwith "Not Implemented"

        member this.Remove (name: string) (network: Network) : unit =
            match store.contents.TryFind name with
            | Some currentNetwork -> store.Value <- Map.add name (Set.difference currentNetwork network) store.contents
            | _ -> failwith "Not Implemented"

        member this.RemoveNetwork(name: string) : unit =
            store.Value <- Map.remove name store.contents

let createInMemoryStore () = InMemoryStore(ref Map.empty)

// let createStore (location: string): IStore =
//     let env = LightningEnvironment(location)
//     env.Open()
