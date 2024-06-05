// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.LigatureStore

open Ligature.Main
open System
open System.Collections.Generic

type LigatureStore =
    abstract member networks: unit -> string seq
    abstract member addNetwork: string -> unit
    abstract member removeNetwork: string -> unit
    // abstract member merge: string -> Network -> Result<unit, LigatureError>
    // abstract member minus: string -> Network -> Result<unit, LigatureError>
    // abstract member query: string -> Network -> Network
    // abstract member trans: string -> Network -> Network -> Network

module InMemoryStore =
    type InMemoryStore(store: Dictionary<string, Set<Statement>>) =
        interface LigatureStore with
            member this.addNetwork networkName =
                store.Add(networkName, Set.empty)

            member this.removeNetwork networkName = store.Remove(networkName) |> ignore
            member this.networks() = store.Keys

            // member this.add name network =
            //     let store = store.Item(name)
            //     store.UnionWith(network.AllStatements ())
            //     Ok(())

            // member this.remove name network =
            //     let store = store.Item(name)
            //     store.ExceptWith(network)
            //     Ok(())

            // member this.read name =
            //     let store = store.Item(name)
            //     store

    let empty () : LigatureStore =
        InMemoryStore(new Dictionary<string, Set<Statement>>())
    let emptyNetwork (): Network = new Network(Set.empty)
