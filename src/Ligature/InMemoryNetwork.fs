// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.InMemoryNetwork

open Ligature.Main
open System
open System.Collections.Generic

let emptyNetwork: Network = Set.empty

// let patternWordToValue (patternWord: PatternWord) : Value =
//     match patternWord with
//     | PatternWord.Word word -> Value.Word word
//     | PatternWord.Slot slot -> Value.Slot slot

// let educeTripleTriple
//     ((entity, attribute, value): Triple)
//     ((patternEntity, patternAttribute, patternValue): Triple)
//     : Option<Map<string, Value>> =
//     let mutable cont = true
//     let mutable result: Map<string, Value> = Map.empty

//     match patternEntity with
//     | PatternWord.Slot(Slot(Some(name))) -> result <- Map.add name (entity |> patternWordToValue) result
//     | PatternWord.Slot(Slot(None)) -> ignore ()
//     | PatternWord.Word _ -> cont <- (patternEntity = entity)

//     if cont then
//         match patternAttribute with
//         | PatternWord.Slot(Slot(Some(name))) ->
//             if Map.containsKey name result then
//                 cont <- (Map.find name result) = (patternWordToValue attribute)
//             else
//                 result <- Map.add name (attribute |> patternWordToValue) result
//         | PatternWord.Slot(Slot(None)) -> ignore ()
//         | PatternWord.Word _ -> cont <- (patternAttribute = attribute)

//     if cont then
//         match patternValue with
//         | Value.Slot(Slot(Some(name))) ->
//             if Map.containsKey name result then
//                 cont <- (Map.find name result) = (value)
//             else
//                 result <- Map.add name (value) result
//         | Value.Slot(Slot(None)) -> ignore ()
//         | _ -> cont <- patternValue = value

//     if cont then Some(result) else None

// let educeNetworkTriple (network: Set<Triple>) (pattern: Triple) : Set<Map<string, Value>> =
//     Set.map (fun triple -> educeTripleTriple triple pattern) network
//     |> Set.fold
//         (fun state values ->
//             match values with
//             | Some(values) -> Set.add values state
//             | None -> state)
//         Set.empty

// let educeNetworkNetwork (network: Set<Triple>) (pattern: Set<Triple>) : Set<Map<string, Value>> =
//     if network.IsEmpty || pattern.IsEmpty then
//         Set.empty
//     else
//         Set.fold
//             (fun state patternTriple -> Set.union (educeNetworkTriple network patternTriple) state)
//             Set.empty
//             pattern


// type InMemoryNetwork(network: Set<Triple>) =
//     let processQueryResults (trans: Network) (values: Set<Map<string, Value>>) : Network =
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

//         member _.Apply(values: Map<string, Value>) =
//             let res: Set<Triple> =
//                 Set.map
//                     (fun ((entity, attribute, value): Triple) ->
//                         match (entity, attribute, value) with
//                         // | { Entity = PatternWord.Word(_)
//                         //     Attribute = PatternWord.Word(_)
//                         //     Value = Value(_) } -> failwith "TODO"
//                         | _ ->
//                             let entity =
//                                 match entity with
//                                 | PatternWord.Word(word) -> word
//                                 | PatternWord.Slot(slot) ->
//                                     match slot with
//                                     | Slot(Some(name)) ->
//                                         match values.TryFind name with
//                                         | Some value ->
//                                             match value with
//                                             | Value.Word word -> word
//                                             | _ -> failwith "Error"
//                                         | None -> failwith "Error"
//                                     | Slot(None) -> failwith "Error"

//                             let attribute =
//                                 match attribute with
//                                 | PatternWord.Word(word) -> word
//                                 | PatternWord.Slot(slot) ->
//                                     match slot with
//                                     | Slot(Some(name)) ->
//                                         match values.TryFind name with
//                                         | Some value ->
//                                             match value with
//                                             | Value.Word word -> word
//                                             | _ -> failwith "Error"
//                                         | None -> failwith "Error"
//                                     | Slot(None) -> failwith "Error"

//                             let value =
//                                 match value with
//                                 | Value.Slot(slot) ->
//                                     match slot with
//                                     | Slot(Some(name)) ->
//                                         match values.TryFind name with
//                                         | Some value -> value
//                                         | None -> failwith "Error"
//                                     | Slot(None) -> failwith "Error"
//                                 | v -> v

//                             (PatternWord.Word(entity), PatternWord.Word(attribute), value))
//                     network

//             InMemoryNetwork(res)

//         member this.Educe pattern : Set<Map<string, Value>> =
//             educeNetworkNetwork network (pattern.Write())

//         member this.Query pattern trans : Network =
//             (this :> Network).Educe pattern |> processQueryResults trans

//         member this.Infer pattern trans : Network =
//             (this :> Network).Query pattern trans |> (this :> Network).Union

// let emptyNetwork: Network = InMemoryNetwork(Set.empty)

// let networkOf (input: Triple seq) : Network = InMemoryNetwork(Set.ofSeq input)
