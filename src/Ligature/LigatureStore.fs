// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.LigatureStore

open Ligature.Main
open System.Collections.Generic

module InMemoryStore =
    type InMemoryStore(store: Dictionary<NetworkName, Network>) =
        interface LigatureStore with
            member this.AddNetwork networkName = store.Add(networkName, Set.empty)

            member this.RemoveNetwork networkName = store.Remove(networkName) |> ignore
            member this.Networks() = store.Keys

            member this.Add name network =
                let oldNetwork = store.Item(name)
                store.Remove(name) |> ignore
                store.Add(name, (Set.union oldNetwork network))
                Ok(())

            member this.Remove name network =
                let oldNetwork = store.Item(name)
                store.Remove(name) |> ignore
                store.Add(name, (Set.difference oldNetwork network))
                Ok(())

            member this.Query name network = failwith "TODO"
            member this.ClearNetwork(arg1: NetworkName) : unit = failwith "Not Implemented"

            member this.Set (arg1: NetworkName) (arg2: Network) : Result<unit, LigatureError> =
                failwith "Not Implemented"
    //                store.Item(name)

    let emptyStore: LigatureStore =
        InMemoryStore(new Dictionary<NetworkName, Set<Statement>>())
