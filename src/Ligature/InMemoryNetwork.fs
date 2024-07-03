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

let educeTripleTriple (triple: Triple) (patternTriple: Triple) : Map<Slot, Value> =
    let mutable cont = true
    let mutable result: Map<Slot, Value> = Map []

    match patternTriple.Entity with
    | PatternIdentifier.Sl(Slot(Some(name))) ->
        result <- Map.add (Slot(Some(name))) (triple.Entity |> patternIdentifierToValue) result
    | PatternIdentifier.Sl(Slot(None)) -> ignore ()
    | PatternIdentifier.Id id -> cont <- (patternTriple.Entity = triple.Entity)
    | _ -> failwith "Error"

    if cont then
        match patternTriple.Attribute with
        | PatternIdentifier.Sl(Slot(Some(name))) ->
            result <- Map.add (Slot(Some(name))) (triple.Attribute |> patternIdentifierToValue) result
        | PatternIdentifier.Sl(Slot(None)) -> ignore ()
        | PatternIdentifier.Id id -> cont <- (patternTriple.Attribute = triple.Attribute)
        | _ -> failwith "Error"

    if cont then
        match patternTriple.Value with
        | Value.Slot(Slot(Some(name))) -> result <- Map.add (Slot(Some(name))) (triple.Value) result
        | Value.Slot(Slot(None)) -> ignore ()
    //| Value.Int(i) -> patternTriple.Value = triple.Value
    //| Value.String(s) -> patternTriple.Value = triple.Value
    //| Value.Bytes(b) -> patternTriple.Value = triple.Value
    //| Value.Identifier(i) -> patternTriple.Value = triple.Value
    // | _ -> cont <- (patternTriple.Value = triple.Value)
    result

let educeNetworkTriple (network: Set<Triple>) (pattern: Triple) : Set<Map<Slot, Value>> =
    Set.map (fun triple -> educeTripleTriple triple pattern) network

let educeNetworkNetwork (network: Set<Triple>) (pattern: Set<Triple>) : Set<Map<Slot, Value>> =
    if network.IsEmpty || pattern.IsEmpty then
        Set.empty
    else
        Set.fold
            (fun state patternTriple -> Set.union (educeNetworkTriple network patternTriple) state)
            Set.empty
            pattern

type InMemoryNetwork(network: Set<Triple>) =
    override this.Equals(other) =
        match other with
        | :? Network as other -> network = other.Write()
        | _ -> false

    interface Network with
        member this.Write() = network

        member this.Count() = Set.count network

        member this.Union other =
            InMemoryNetwork(Set.union network (other.Write()))

        member this.Minus other =
            InMemoryNetwork(Set.difference (other.Write()) network)

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
            educeNetworkNetwork network (pattern.Write())

        member this.Query pattern trans : Network = failwith "TODO"

        member this.Infer pattern trans : Network = failwith "TODO"

let emptyNetwork: Network = InMemoryNetwork(Set.empty)

let networkOf (input: Triple seq) : Network = InMemoryNetwork(Set.ofSeq input)
