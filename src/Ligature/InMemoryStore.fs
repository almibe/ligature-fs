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
                | Extension { concept = concept } -> Set.add concept state
                | _ -> state)
            Set.empty
            entries

    Set.fold
        (fun state (entry: Entry) ->
            match entry with
            | Role { first = first; second = second } -> (concepts.Contains first) && (concepts.Contains second)
            | _ -> state)
        true
        entries

let consistent (aBox: Set<Entry>) : bool =
    let mutable individuals: Map<Element, Set<Entry>> = Map.empty

    Set.fold
        (fun state (entry: Entry) ->
            match state with
            | false -> false
            | true ->
                match entry with
                | Extension { concept = conceptName
                              element = symbol } ->
                    let concept =
                        Extension
                            { concept = conceptName
                              element = symbol }

                    let notVersion =
                        NonExtension
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
                | NonExtension { concept = concept; element = symbol } ->
                    let notConcept = NonExtension { concept = concept; element = symbol }
                    let inverse = Extension { concept = concept; element = symbol }

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
                | Role _ -> true)
        true
        aBox

type InMemoryStore(store: Dictionary<NetworkName, Set<Entry>>) =
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
            | (false, _) -> failwith "TODO"

        member _.Set name network : Result<unit, LigatureError> =
            store.Add(name, network) |> ignore
            Ok(())

        member _.AllConcepts(networkName: NetworkName) : Set<ConceptName> =
            match store.TryGetValue(networkName) with
            | true, entries -> Set.fold (fun state value -> state) Set.empty entries
            | false, _ -> failwith "Not Implemented"

        member _.AllExtentions (networkName: NetworkName) (conceptName: ConceptName) : Set<Element> =
            match store.TryGetValue(networkName) with
            | true, entries ->
                Set.fold
                    (fun state value ->
                        match value with
                        | Extension ex ->
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
                        | Role role -> if role.role = roleName then Set.add role state else state
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
                        | Role role -> Set.add role.role state
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
            | true, entries -> consistent entries
            | false, _ -> failwith "Not Implemented"

        member _.Find (networkName: NetworkName) (terms: Set<QueryTerm>) : Set<Map<Variable, Element>> =
            failwith "Not Implemented"

let emptyInMemoryStore () : LigatureStore =
    InMemoryStore(new Dictionary<NetworkName, Set<Entry>>())

// let patternNameToIdentifier (patternName: Pattern) : Pattern =
//     match patternName with
//     | Pattern.Symbol path -> Pattern.Symbol path
//     | Pattern.Slot slot -> Pattern.Slot slot

// let matchStatementStatement
//     ((patternEntity, patternAttribute, patternIdentifier): Statement)
//     ((entity, attribute, value): Statement)
//     : Option<Map<string, Pattern>> =
//     failwith "TODO"
// let mutable cont = true
// let mutable result: Map<string, Pattern> = Map.empty

// match patternEntity with
// | Pattern.Slot(Slot(Some(name))) -> result <- Map.add name (entity |> patternNameToIdentifier) result
// | Pattern.Slot(Slot(None)) -> ignore ()
// | Pattern.Symbol _ -> cont <- (patternEntity = entity)

// if cont then
//     match patternAttribute with
//     | Pattern.Slot(Slot(Some(name))) ->
//         if Map.containsKey name result then
//             cont <- (Map.find name result) = (patternNameToIdentifier attribute)
//         else
//             result <- Map.add name (attribute |> patternNameToIdentifier) result
//     | Pattern.Slot(Slot(None)) -> ignore ()
//     | Pattern.Symbol _ -> cont <- (patternAttribute = attribute)

// if cont then
//     match patternIdentifier with
//     | Pattern.Slot(Slot(Some(name))) ->
//         if Map.containsKey name result then
//             cont <- (Map.find name result) = (value)
//         else
//             result <- Map.add name (value) result
//     | Pattern.Slot(Slot(None)) -> ignore ()
//     | _ -> cont <- patternIdentifier = value

// if cont then Some(result) else None

// let matchNetworkStatement (network: Set<Statement>) (pattern: Statement) : Set<Map<string, Pattern>> =
//     Set.map (fun triple -> matchStatementStatement triple pattern) network
//     |> Set.fold
//         (fun state values ->
//             match values with
//             | Some(values) -> Set.add values state
//             | None -> state)
//         Set.empty

// let matchNetworkNetwork (network: Network) (pattern: Network) : Set<Map<string, Pattern>> =
//     if network.IsEmpty || pattern.IsEmpty then
//         Set.empty
//     else
//         Set.fold
//             (fun state patternStatement -> Set.union (matchNetworkStatement network patternStatement) state)
//             Set.empty
//             pattern

//let mapToNetwork (input: Map<string, Pattern>) : Network = failwith "TODO"
// Map.toList input
// |> Set.ofList
// |> Set.map (fun (name, value) -> (Pattern.Slot((Slot(Some(name)))), Pattern.Symbol(Symbol("=")), value))

// let matchNetwork (input: Network) (pattern: Network) : WanderValue =
//     matchNetworkNetwork input pattern
//     |> Set.toList
//     |> List.map mapToNetwork
//     |> List.map WanderValue.Network
//     |> WanderValue.Quote

// type InMemoryNetwork(network: Set<Statement>) =
//     let processQueryResults (trans: Network) (values: Set<Map<string, Identifier>>) : Network =
//         List.ofSeq values
//         |> List.map (fun values -> trans.Apply values)
//         |> List.fold (fun state network -> state.Union network) (InMemoryNetwork(Set.empty))

//     override _.Equals(other) =
//         match other with
//         | :? Network as other -> network = other.Write()
//         | _ -> false

//     override _.GetHashCode() = network.GetHashCode()

//     interface Network with
//         member _.Write() = network

//         member _.Count() = Set.count network

//         member _.Union other =
//             InMemoryNetwork(Set.union network (other.Write()))

//         member _.Minus other =
//             InMemoryNetwork(Set.difference (other.Write()) network)

//         member _.Apply(values: Map<string, Identifier>) =
//             let res: Set<Statement> =
//                 Set.map
//                     (fun ((entity, attribute, value): Statement) ->
//                         match (entity, attribute, value) with
//                         // | { Entity = Pattern.Name(_)
//                         //     Attribute = Pattern.Name(_)
//                         //     Identifier = Identifier(_) } -> failwith "TODO"
//                         | _ ->
//                             let entity =
//                                 match entity with
//                                 | Pattern.Name(identifier) -> identifier
//                                 | Pattern.Slot(slot) ->
//                                     match slot with
//                                     | Slot(Some(name)) ->
//                                         match values.TryFind name with
//                                         | Some value ->
//                                             match value with
//                                             | Identifier.Name identifier -> identifier
//                                             | _ -> failwith "Error"
//                                         | None -> failwith "Error"
//                                     | Slot(None) -> failwith "Error"

//                             let attribute =
//                                 match attribute with
//                                 | Pattern.Name(identifier) -> identifier
//                                 | Pattern.Slot(slot) ->
//                                     match slot with
//                                     | Slot(Some(name)) ->
//                                         match values.TryFind name with
//                                         | Some value ->
//                                             match value with
//                                             | Identifier.Name identifier -> identifier
//                                             | _ -> failwith "Error"
//                                         | None -> failwith "Error"
//                                     | Slot(None) -> failwith "Error"

//                             let value =
//                                 match value with
//                                 | Identifier.Slot(slot) ->
//                                     match slot with
//                                     | Slot(Some(name)) ->
//                                         match values.TryFind name with
//                                         | Some value -> value
//                                         | None -> failwith "Error"
//                                     | Slot(None) -> failwith "Error"
//                                 | v -> v

//                             (Pattern.Name(entity), Pattern.Name(attribute), value))
//                     network

//             InMemoryNetwork(res)

//         member this.Educe pattern : Set<Map<string, Identifier>> =
//             matchNetworkNetwork network (pattern.Write())

//         member this.Query pattern trans : Network =
//             (this :> Network).Educe pattern |> processQueryResults trans

//         member this.Infer pattern trans : Network =
//             (this :> Network).Query pattern trans |> (this :> Network).Union

// let emptyNetwork: Network = InMemoryNetwork(Set.empty)

// let networkOf (input: Statement seq) : Network = InMemoryNetwork(Set.ofSeq input)
