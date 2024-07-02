// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.InMemoryNetwork

open Ligature.Main
open System
open System.Collections.Generic

let patternIdentifierToValue (patternIdentifier: PatternIdentifier) : Value =
    match patternIdentifier with
    | PatternIdentifier.Id identifier -> Value.Identifier identifier
    | PatternIdentifier.Sl slot -> Value.Slot slot

let educeTripleTriple (triple: Triple) (patternTriple: Triple) : Map<Slot, Value> option =
    let mutable cont = true
    let mutable result: Map<Slot, Value> = Map []

    // if (isSlot(patternTriple.entity)) {
    //     const slot = patternTriple.entity as Slot
    //     if (slot.name != null) {
    //         result = result.set(slot, triple.entity)
    //     }
    // } else if (triple.entity != patternTriple.entity) {
    //     return null
    // }

    match patternTriple.Entity with
    | PatternIdentifier.Sl(Slot(Some(name))) ->
        result <- Map.add (Slot(Some(name))) (triple.Entity |> patternIdentifierToValue) result
    | PatternIdentifier.Sl(Slot(None)) -> ignore ()
    | PatternIdentifier.Id id -> cont <- (patternTriple.Entity = triple.Entity)
    | _ -> failwith "Error"

    // if (isSlot(patternTriple.attribute)) {
    //     const slot = patternTriple.attribute as Slot
    //     if (slot.name != null) {
    //         result = result.set(slot, triple.attribute)
    //     }
    // } else if (triple.attribute != patternTriple.attribute) {
    //     return null
    // }

    if cont then
        match patternTriple.Attribute with
        | PatternIdentifier.Sl(Slot(Some(name))) ->
            result <- Map.add (Slot(Some(name))) (triple.Attribute |> patternIdentifierToValue) result
        | PatternIdentifier.Sl(Slot(None)) -> ignore ()
        | PatternIdentifier.Id id -> cont <- (patternTriple.Attribute = triple.Attribute)
        | _ -> failwith "Error"

    // if (isSlot(patternTriple.value)) {
    //     const slot = patternTriple.value as Slot
    //     if (slot.name != null) {
    //         result = result.set(slot, triple.value)
    //     }
    // } else if (triple.value != patternTriple.value ) {
    //     return null
    // }
    // result

    if cont then
        match patternTriple.Attribute with
        | PatternIdentifier.Sl(Slot(Some(name))) ->
            result <- Map.add (Slot(Some(name))) (triple.Attribute |> patternIdentifierToValue) result
        | PatternIdentifier.Sl(Slot(None)) -> ignore ()
        | PatternIdentifier.Id id -> cont <- (patternTriple.Attribute = triple.Attribute)
        | _ -> failwith "Error"

    if cont then Some result else None

let educeNetworkTriple (network: Set<Triple>) (pattern: Triple) : Set<Map<Slot, Value>> option =
    let res: Set<Option<Map<Slot, Value>>> =
        Set.map (fun triple -> educeTripleTriple triple pattern) network

    if res.IsEmpty then
        Some(Set [])
    else
        let filtered =
            Set.filter (fun (value: Option<Map<Slot, Value>>) -> value.IsSome) res
            |> Set.map (fun (value: Option<Map<Slot, Value>>) -> value.Value)

        if filtered.IsEmpty then None else Some(filtered)

let educeNetworkNetwork (network: Set<Triple>) (pattern: Set<Triple>) : Set<Map<Slot, Value>> =
    if network.IsEmpty || pattern.IsEmpty then
        Set.empty
    else
        let mutable res: Set<Map<Slot, Value>> = Set.empty
        let mutable currentNames: Map<Slot, Value> = Map.empty

        network
        |> Set.iter (fun triple ->
            currentNames <- Map.empty //reset state

            pattern
            |> Set.iter (fun (pattern: Triple) ->
                let mutable matched = true

                match pattern.Entity with
                | PatternIdentifier.Id identifier -> matched <- triple.Entity = PatternIdentifier.Id identifier
                | PatternIdentifier.Sl slot ->
                    match slot with
                    | Slot(Some(name)) ->
                        if currentNames.ContainsKey slot then
                            failwith "TODO"
                        else
                            match triple.Entity with
                            | PatternIdentifier.Id identifier ->
                                currentNames <- currentNames.Add(slot, Value.Identifier identifier)
                            | PatternIdentifier.Sl slot -> failwith "Error"
                    | Slot(None) -> ()

                match pattern.Attribute with
                | PatternIdentifier.Id identifier -> matched <- triple.Attribute = PatternIdentifier.Id identifier
                | PatternIdentifier.Sl slot ->
                    match slot with
                    | Slot(Some(name)) ->
                        if currentNames.ContainsKey slot then
                            failwith "TODO"
                        else
                            match triple.Attribute with
                            | PatternIdentifier.Id identifier ->
                                currentNames <- currentNames.Add(slot, Value.Identifier identifier)
                            | _ -> failwith "Error"
                    | Slot(None) -> ()

                match pattern.Value with
                | Value.Slot slot ->
                    match slot with
                    | Slot(Some(name)) ->
                        if currentNames.ContainsKey slot then
                            failwith "TODO"
                        else
                            currentNames <- currentNames.Add(slot, triple.Value)
                    | Slot(None) -> ()

                | value -> matched <- triple.Value = value

                if matched then
                    res <- Set.add currentNames res))

        res

type InMemoryNetwork(network: Set<Triple>) =
    interface Network with
        member this.Write() = network

        member this.Count() = Set.count network

        member this.Union other =
            InMemoryNetwork(Set.union network (other.Write()))

        member this.Minus other =
            InMemoryNetwork(Set.difference network (other.Write()))

        member this.Apply(values: Map<Slot, Value>) =
            let res: Set<Triple> =
                Set.map
                    (fun (triple: Triple) ->
                        match triple with
                        // | { Entity = PatternIdentifier.Id(_)
                        //     Attribute = PatternIdentifier.Id(_)
                        //     Value = Value(_) } -> failwith "TODO"
                        | _ ->
                            let entity =
                                match triple.Entity with
                                | PatternIdentifier.Id(identifier) -> identifier
                                | PatternIdentifier.Sl(slot) ->
                                    match slot with
                                    | Slot(Some(name)) ->
                                        match values.TryFind slot with
                                        | Some value ->
                                            match value with
                                            | Value.Identifier identifier -> identifier
                                            | _ -> failwith "Error"
                                        | None -> failwith "Error"
                                    | Slot(None) -> failwith "Error"

                            let attribute =
                                match triple.Attribute with
                                | PatternIdentifier.Id(identifier) -> identifier
                                | PatternIdentifier.Sl(slot) ->
                                    match slot with
                                    | Slot(Some(name)) ->
                                        match values.TryFind slot with
                                        | Some value ->
                                            match value with
                                            | Value.Identifier identifier -> identifier
                                            | _ -> failwith "Error"
                                        | None -> failwith "Error"
                                    | Slot(None) -> failwith "Error"

                            let value =
                                match triple.Value with
                                | Value.Slot(slot) ->
                                    match slot with
                                    | Slot(Some(name)) ->
                                        match values.TryFind slot with
                                        | Some value -> value
                                        | None -> failwith "Error"
                                    | Slot(None) -> failwith "Error"
                                | v -> v

                            { Entity = PatternIdentifier.Id entity
                              Attribute = PatternIdentifier.Id attribute
                              Value = value })
                    network

            InMemoryNetwork(res)

        member this.Educe pattern : Set<Map<Slot, Value>> =
            educeNetworkNetwork network (pattern.Write()) |> Set.ofSeq

        member this.Query pattern trans : Network = failwith "TODO"

        member this.Infer pattern trans : Network = failwith "TODO"

let empty () = InMemoryNetwork(Set.empty)
