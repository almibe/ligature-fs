// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.InMemoryNetwork

open Ligature.Main
open System
open System.Collections.Generic

let emptyNetwork: Network = Set.empty

let patternNameToLigatureValue (patternName: PatternName) : LigatureValue =
    match patternName with
    | PatternName.Name path -> LigatureValue.Name path
    | PatternName.Slot slot -> LigatureValue.Slot slot

let educeStatementStatement
    ((entity, attribute, value): Statement)
    ((patternEntity, patternAttribute, patternLigatureValue): Statement)
    : Option<Map<string, LigatureValue>> =
    let mutable cont = true
    let mutable result: Map<string, LigatureValue> = Map.empty

    match patternEntity with
    | PatternName.Slot(Slot(Some(name))) -> result <- Map.add name (entity |> patternNameToLigatureValue) result
    | PatternName.Slot(Slot(None)) -> ignore ()
    | PatternName.Name _ -> cont <- (patternEntity = entity)

    if cont then
        match patternAttribute with
        | PatternName.Slot(Slot(Some(name))) ->
            if Map.containsKey name result then
                cont <- (Map.find name result) = (patternNameToLigatureValue attribute)
            else
                result <- Map.add name (attribute |> patternNameToLigatureValue) result
        | PatternName.Slot(Slot(None)) -> ignore ()
        | PatternName.Name _ -> cont <- (patternAttribute = attribute)

    if cont then
        match patternLigatureValue with
        | LigatureValue.Slot(Slot(Some(name))) ->
            if Map.containsKey name result then
                cont <- (Map.find name result) = (value)
            else
                result <- Map.add name (value) result
        | LigatureValue.Slot(Slot(None)) -> ignore ()
        | _ -> cont <- patternLigatureValue = value

    if cont then Some(result) else None

let educeNetworkStatement (network: Set<Statement>) (pattern: Statement) : Set<Map<string, LigatureValue>> =
    Set.map (fun triple -> educeStatementStatement triple pattern) network
    |> Set.fold
        (fun state values ->
            match values with
            | Some(values) -> Set.add values state
            | None -> state)
        Set.empty

let educeNetworkNetwork (network: Set<Statement>) (pattern: Set<Statement>) : Set<Map<string, LigatureValue>> =
    if network.IsEmpty || pattern.IsEmpty then
        Set.empty
    else
        Set.fold
            (fun state patternStatement -> Set.union (educeNetworkStatement network patternStatement) state)
            Set.empty
            pattern

// type InMemoryNetwork(network: Set<Statement>) =
//     let processQueryResults (trans: Network) (values: Set<Map<string, LigatureValue>>) : Network =
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

//         member _.Apply(values: Map<string, LigatureValue>) =
//             let res: Set<Statement> =
//                 Set.map
//                     (fun ((entity, attribute, value): Statement) ->
//                         match (entity, attribute, value) with
//                         // | { Entity = PatternName.Name(_)
//                         //     Attribute = PatternName.Name(_)
//                         //     LigatureValue = LigatureValue(_) } -> failwith "TODO"
//                         | _ ->
//                             let entity =
//                                 match entity with
//                                 | PatternName.Name(identifier) -> identifier
//                                 | PatternName.Slot(slot) ->
//                                     match slot with
//                                     | Slot(Some(name)) ->
//                                         match values.TryFind name with
//                                         | Some value ->
//                                             match value with
//                                             | LigatureValue.Name identifier -> identifier
//                                             | _ -> failwith "Error"
//                                         | None -> failwith "Error"
//                                     | Slot(None) -> failwith "Error"

//                             let attribute =
//                                 match attribute with
//                                 | PatternName.Name(identifier) -> identifier
//                                 | PatternName.Slot(slot) ->
//                                     match slot with
//                                     | Slot(Some(name)) ->
//                                         match values.TryFind name with
//                                         | Some value ->
//                                             match value with
//                                             | LigatureValue.Name identifier -> identifier
//                                             | _ -> failwith "Error"
//                                         | None -> failwith "Error"
//                                     | Slot(None) -> failwith "Error"

//                             let value =
//                                 match value with
//                                 | LigatureValue.Slot(slot) ->
//                                     match slot with
//                                     | Slot(Some(name)) ->
//                                         match values.TryFind name with
//                                         | Some value -> value
//                                         | None -> failwith "Error"
//                                     | Slot(None) -> failwith "Error"
//                                 | v -> v

//                             (PatternName.Name(entity), PatternName.Name(attribute), value))
//                     network

//             InMemoryNetwork(res)

//         member this.Educe pattern : Set<Map<string, LigatureValue>> =
//             educeNetworkNetwork network (pattern.Write())

//         member this.Query pattern trans : Network =
//             (this :> Network).Educe pattern |> processQueryResults trans

//         member this.Infer pattern trans : Network =
//             (this :> Network).Query pattern trans |> (this :> Network).Union

// let emptyNetwork: Network = InMemoryNetwork(Set.empty)

// let networkOf (input: Statement seq) : Network = InMemoryNetwork(Set.ofSeq input)
