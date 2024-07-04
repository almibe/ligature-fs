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

let educeTripleTriple (triple: Triple) (patternTriple: Triple) : Option<Map<string, Value>> =
    let mutable cont = true
    let mutable result: Map<string, Value> = Map.empty

    match patternTriple.Entity with
    | PatternIdentifier.Sl(Slot(Some(name))) ->
        result <- Map.add name (triple.Entity |> patternIdentifierToValue) result
    | PatternIdentifier.Sl(Slot(None)) -> ignore ()
    | PatternIdentifier.Id id -> cont <- (patternTriple.Entity = triple.Entity)

    if cont then
        match patternTriple.Attribute with
        | PatternIdentifier.Sl(Slot(Some(name))) ->
            if Map.containsKey name result then
                cont <- (Map.find name result) = (patternIdentifierToValue triple.Attribute)
            else
                result <- Map.add name (triple.Attribute |> patternIdentifierToValue) result
        | PatternIdentifier.Sl(Slot(None)) -> ignore ()
        | PatternIdentifier.Id id -> cont <- (patternTriple.Attribute = triple.Attribute)

    if cont then
        match patternTriple.Value with
        | Value.Slot(Slot(Some(name))) ->
            if Map.containsKey name result then
                cont <- (Map.find name result) = (triple.Value)
            else
                result <- Map.add name (triple.Value) result
        | Value.Slot(Slot(None)) -> ignore ()
        | _ -> cont <- patternTriple.Value = triple.Value

    if cont then Some(result) else None

let educeNetworkTriple (network: Set<Triple>) (pattern: Triple) : Set<Map<string, Value>> =
    Set.map (fun triple -> educeTripleTriple triple pattern) network
    |> Set.fold
        (fun state values ->
            match values with
            | Some(values) -> Set.add values state
            | None -> state)
        Set.empty

let educeNetworkNetwork (network: Set<Triple>) (pattern: Set<Triple>) : Set<Map<string, Value>> =
    if network.IsEmpty || pattern.IsEmpty then
        Set.empty
    else
        Set.fold
            (fun state patternTriple -> Set.union (educeNetworkTriple network patternTriple) state)
            Set.empty
            pattern


type InMemoryNetwork(network: Set<Triple>) =
    let processQueryResults (trans: Network) (values: Set<Map<string, Value>>): Network =
        List.ofSeq values
        |> List.map (fun values -> trans.Apply values)
        |> List.fold (fun state network -> state.Union network) (InMemoryNetwork(Set.empty))

    override _.Equals(other) =
        match other with
        | :? Network as other -> network = other.Write()
        | _ -> false

    override _.GetHashCode () =
        network.GetHashCode()
    interface Network with
        member _.Write() = network

        member _.Count() = Set.count network

        member _.Union other =
            InMemoryNetwork(Set.union network (other.Write()))

        member _.Minus other =
            InMemoryNetwork(Set.difference (other.Write()) network)

        member _.Apply(values: Map<string, Value>) =
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
                                        match values.TryFind name with
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
                                        match values.TryFind name with
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
                                        match values.TryFind name with
                                        | Some value -> value
                                        | None -> failwith "Error"
                                    | Slot(None) -> failwith "Error"
                                | v -> v

                            { Entity = PatternIdentifier.Id entity
                              Attribute = PatternIdentifier.Id attribute
                              Value = value })
                    network

            InMemoryNetwork(res)

        member this.Educe pattern : Set<Map<string, Value>> =
            educeNetworkNetwork network (pattern.Write())

        member this.Query pattern trans : Network = 
            (this :> Network).Educe pattern
            |> processQueryResults trans

        member this.Infer pattern trans : Network =
            (this :> Network).Query pattern trans
            |> (this :> Network).Union

let emptyNetwork: Network = InMemoryNetwork(Set.empty)

let networkOf (input: Triple seq) : Network = InMemoryNetwork(Set.ofSeq input)
