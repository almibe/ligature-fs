// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.LigatureStore

open Ligature.Main
open System.Collections.Generic

module InMemoryStore =
    type InMemoryStore(store: Dictionary<Name, Network>) =
        interface LigatureStore with
            member _.AddNetwork networkName = store.Add(networkName, Set.empty)

            member _.RemoveNetwork networkName = store.Remove(networkName) |> ignore
            member _.Networks() = store.Keys

            member _.Add name network =
                match store.TryGetValue name with
                | (true, network) ->
                    let oldNetwork = store.Item(name)
                    store.Remove(name) |> ignore
                    store.Add(name, (Set.union oldNetwork network))
                    Ok(())
                | (false, _) ->
                    store.Add(name, network)
                    Ok(())

            member _.Remove name network =
                let oldNetwork = store.Item(name)
                store.Remove(name) |> ignore
                store.Add(name, (Set.difference oldNetwork network))
                Ok(())

            member _.Read name = store.Item(name)

            member _.Query name network = failwith "TODO"
            member _.ClearNetwork(arg1: Name) : unit = failwith "Not Implemented"

            member _.Set name network : Result<unit, LigatureError> =
                store.Add(name, network) |> ignore
                Ok(())
    //                store.Item(name)

    let emptyStore: LigatureStore =
        InMemoryStore(new Dictionary<Name, Set<Statement>>())
