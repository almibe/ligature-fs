// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.InMemoryNetwork

open Ligature.Main
open System
open System.Collections.Generic

let educe (network: Set<Triple>) (pattern: Set<Triple>) : Map<Slot, Value> list =
    if network.IsEmpty || pattern.IsEmpty then
        List.Empty
    else
        let mutable res: Map<Slot, Value> list = List.empty //TODO make a list
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
                    if slot.Named then
                        if currentNames.ContainsKey slot then
                            failwith "TODO"
                        else
                            match triple.Entity with
                            | PatternIdentifier.Id identifier ->
                                currentNames <- currentNames.Add(slot, Value.Identifier identifier)
                            | PatternIdentifier.Sl slot -> failwith "Error"
                    else
                        ()

                match pattern.Attribute with
                | PatternIdentifier.Id identifier -> matched <- triple.Attribute = PatternIdentifier.Id identifier
                | PatternIdentifier.Sl slot ->
                    if slot.Named then
                        if currentNames.ContainsKey slot then
                            failwith "TODO"
                        else
                            match triple.Attribute with
                            | PatternIdentifier.Id identifier ->
                                currentNames <- currentNames.Add(slot, Value.Identifier identifier)
                            | _ -> failwith "Error"
                    else
                        ()

                match pattern.Value with
                | Value.Slot slot ->
                    if slot.Named then
                        if currentNames.ContainsKey slot then
                            failwith "TODO"
                        else
                            currentNames <- currentNames.Add(slot, triple.Value)
                    else
                        ()
                | value -> matched <- triple.Value = value

                if matched then
                    res <- List.append res [ currentNames ]))

        List.ofSeq res

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
                                    if slot.Named then
                                        match values.TryFind slot with
                                        | Some value ->
                                            match value with
                                            | Value.Identifier identifier -> identifier
                                            | _ -> failwith "Error"
                                        | None -> failwith "Error"
                                    else
                                        failwith "Error"

                            let attribute =
                                match triple.Attribute with
                                | PatternIdentifier.Id(identifier) -> identifier
                                | PatternIdentifier.Sl(slot) ->
                                    if slot.Named then
                                        match values.TryFind slot with
                                        | Some value ->
                                            match value with
                                            | Value.Identifier identifier -> identifier
                                            | _ -> failwith "Error"
                                        | None -> failwith "Error"
                                    else
                                        failwith "Error"

                            let value =
                                match triple.Value with
                                | Value.Slot(slot) ->
                                    if slot.Named then
                                        match values.TryFind slot with
                                        | Some value -> value
                                        | None -> failwith "Error"
                                    else
                                        failwith "Error"
                                | v -> v

                            { Entity = PatternIdentifier.Id entity
                              Attribute = PatternIdentifier.Id attribute
                              Value = value })
                    network
            InMemoryNetwork(res)

        member this.Educe pattern : Set<Map<Slot, Value>> = educe network (pattern.Write()) |> Set.ofSeq

        member this.Query pattern trans : Network = failwith "TODO"

        member this.Infer pattern trans : Network = failwith "TODO"

let empty () = InMemoryNetwork(Set.empty)
