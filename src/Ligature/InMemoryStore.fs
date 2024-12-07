// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.InMemoryStore

open Ligature.Main
open System.Collections.Generic

let isComplete (entries: Set<Entry>) : bool =
    let concepts =
        Set.fold
            (fun state value ->
                match value with
                | Entry.Extends { concept = concept } -> Set.add concept state
                | _ -> state)
            Set.empty
            entries

    Set.fold
        (fun state (entry: Entry) ->
            match entry with
            | Entry.Role { first = first; second = second } -> (concepts.Contains first) && (concepts.Contains second)
            | _ -> state)
        true
        entries

let isConsistent (network: Network) : bool =
    let mutable individuals: Map<Symbol, Set<Entry>> = Map.empty

    Set.fold
        (fun state (entry: Entry) ->
            match state with
            | false -> false
            | true ->
                match entry with
                | Entry.Extends { concept = conceptName
                                  element = symbol } ->
                    let concept =
                        Entry.Extends
                            { concept = conceptName
                              element = symbol }

                    let notVersion =
                        Entry.NotExtends
                            { concept = conceptName
                              element = symbol }

                    match individuals.TryFind symbol with
                    | None ->
                        individuals <- Map.add symbol (Set.ofList [ concept ]) individuals
                        true
                    | Some res ->
                        if res.Contains(notVersion) then
                            false
                        else
                            individuals <- Map.add symbol (Set.add (concept) res) individuals
                            true
                | Entry.NotExtends { concept = concept; element = symbol } ->
                    let notConcept = Entry.NotExtends { concept = concept; element = symbol }
                    let inverse = Entry.Extends { concept = concept; element = symbol }

                    match individuals.TryFind symbol with
                    | None ->
                        individuals <- Map.add symbol (Set.ofList [ notConcept ]) individuals
                        true
                    | Some res ->
                        if res.Contains(inverse) then
                            false
                        else
                            individuals <- Map.add symbol (Set.add notConcept res) individuals
                            true
                | Entry.Role _ -> true)
        true
        network

type InMemoryStore(store: Dictionary<NetworkName, Set<Entry>>) =
    interface System.IDisposable with
        member _.Dispose() : unit = ()

    interface LigatureStore with
        member _.AddNetwork networkName = Ok(store.Add(networkName, Set.empty))

        member _.RemoveNetwork networkName =
            store.Remove(networkName) |> ignore
            Ok()

        member _.Networks() = store.Keys |> Set.ofSeq |> Ok

        member _.AddEntries name network =
            match store.TryGetValue name with
            | (true, network) ->
                let oldNetwork = store.Item(name)
                store.Remove(name) |> ignore
                store.Add(name, (Set.union oldNetwork network))
                Ok(())
            | (false, _) ->
                store.Add(name, network)
                Ok(())

        member _.RemoveEntries name network =
            let oldNetwork = store.Item(name)
            store.Remove(name) |> ignore
            store.Add(name, (Set.difference oldNetwork network))
            Ok(())

        member _.ReadNetwork(networkName: NetworkName) : Result<Set<Entry>, LigatureError> =
            match store.TryGetValue networkName with
            | (true, network) -> Ok network
            | (false, _) -> error "Network not found." None

        member _.SetNetwork name network : Result<unit, LigatureError> =
            store.Add(name, network) |> ignore
            Ok(())

        member _.FilterEntries (networkName: NetworkName) (terms: Set<Entry>) : Result<Network, LigatureError> =
            failwith "Not Implemented"

let emptyInMemoryStore () : LigatureStore =
    new InMemoryStore(new Dictionary<NetworkName, Set<Entry>>())
