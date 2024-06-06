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
    abstract member add: string -> Network -> Result<unit, LigatureError>
    abstract member remove: string -> Network -> Result<unit, LigatureError>
    abstract member read: string -> Network

module InMemoryStore =
    type InMemoryStore(store: Dictionary<string, Set<Statement>>) =
        interface LigatureStore with
            member this.addNetwork networkName = store.Add(networkName, Set.empty)

            member this.removeNetwork networkName = store.Remove(networkName) |> ignore
            member this.networks() = store.Keys

            member this.add name network =
                let oldNetwork = store.Item(name)
                store.Remove(name)
                store.Add(name, (Set.union oldNetwork (network.AllStatements ())))
                Ok(())

            member this.remove name network =
                let oldNetwork = store.Item(name)
                store.Remove(name)
                store.Add(name, (Set.difference oldNetwork (network.AllStatements ())))
                Ok(())

            member this.read name =
                let store = store.Item(name)
                Network(store)

    let empty () : LigatureStore =
        InMemoryStore(new Dictionary<string, Set<Statement>>())

    let emptyNetwork: Network = new Network(Set.empty)
