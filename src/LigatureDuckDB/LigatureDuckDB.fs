// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.DuckDB

open Ligature.Main

type LigatureDuckDB() =
    interface LigatureStore with
        member _.AddNetwork networkName = store.Add(networkName, Set.empty)

        member _.RemoveNetwork networkName = store.Remove(networkName) |> ignore
        member _.Networks() = store.Keys |> Set.ofSeq

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

        member _.ClearNetwork networkName : unit = store.Remove networkName |> ignore

        member _.Read(networkName: NetworkName) : Set<Entry> =
            match store.TryGetValue networkName with
            | (true, network) -> network
            | (false, _) -> Set.empty

        member _.Set name network : Result<unit, LigatureError> =
            store.Add(name, network) |> ignore
            Ok(())

        member _.AllConcepts(networkName: NetworkName) : Set<ConceptName> =
            match store.TryGetValue(networkName) with
            | true, entries -> Set.fold (fun state value -> state) Set.empty entries
            | false, _ -> failwith "Not Implemented"

        member _.AllExtentions (networkName: NetworkName) (conceptName: ConceptName) : Set<Symbol> =
            match store.TryGetValue(networkName) with
            | true, entries ->
                Set.fold
                    (fun state value ->
                        match value with
                        | Entry.Extension ex ->
                            if ex.concept = conceptName then
                                Set.add ex.element state
                            else
                                state
                        | _ -> state)
                    Set.empty
                    entries
            | false, _ -> failwith "Not Implemented"

        member _.AllRoleInstances (networkName: NetworkName) (roleName: RoleName) : Set<Role> =
            match store.TryGetValue(networkName) with
            | true, entries ->
                Set.fold
                    (fun state value ->
                        match value with
                        | Entry.Role role -> if role.role = roleName then Set.add role state else state
                        | _ -> state)
                    Set.empty
                    entries
            | false, _ -> failwith "Not Implemented"

        member _.AllRoles(networkName: NetworkName) : Set<RoleName> =
            match store.TryGetValue(networkName) with
            | true, entries ->
                Set.fold
                    (fun state value ->
                        match value with
                        | Entry.Role role -> Set.add role.role state
                        | _ -> state)
                    Set.empty
                    entries
            | false, _ -> failwith "Not Implemented"

        member _.IsComplete(networkName: NetworkName) : bool =
            match store.TryGetValue(networkName) with
            | true, entries -> isComplete entries
            | false, _ -> failwith "Not Implemented"

        member _.IsConsistent(networkName: NetworkName) : bool =
            match store.TryGetValue(networkName) with
            | true, entries -> isConsistent entries
            | false, _ -> failwith "Not Implemented"

        member _.Find (networkName: NetworkName) (terms: Set<Entry>) : Set<Map<Element, Symbol>> =
            failwith "Not Implemented"

let inMemoryStore () : LigatureStore =
    using var duckDBConnection = new DuckDBConnection("DataSource=:memory:")
