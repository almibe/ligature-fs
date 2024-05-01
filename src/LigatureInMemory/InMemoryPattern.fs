// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.InMemory.Pattern

open Ligature
open Dataset

type InMemoryPattern(patternStatements: Set<PatternStatement>) =
    override _.Equals(other) =
        match other with
        | :? IPattern as other -> patternStatements = other.PatternStatements
        | _ ->
            false

    override _.GetHashCode() = patternStatements.GetHashCode()

    interface IPattern with
        member _.PatternStatements = patternStatements

        member _.Apply(data: Map<Slot, Value>) =
            let res: Set<Statement> =
                Set.map
                    (fun (statement: PatternStatement) ->
                        match statement with
                        | { Entity = PatternIdentifier.Identifier(_)
                            Attribute = PatternIdentifier.Identifier(_)
                            Value = PatternValue.Value(_) } -> failwith "TODO"
                        | _ ->
                            let entity =
                                match statement.Entity with
                                | PatternIdentifier.Identifier(identifier) -> identifier
                                | PatternIdentifier.Slot(slot) ->
                                    if slot.Named then
                                        match data.TryFind slot with
                                        | Some value ->
                                            match value with
                                            | Value.Identifier identifier -> identifier
                                            | _ -> failwith "Error"
                                        | None -> failwith "Error"
                                    else
                                        failwith "Error"

                            let attribute =
                                match statement.Attribute with
                                | PatternIdentifier.Identifier(identifier) -> identifier
                                | PatternIdentifier.Slot(slot) ->
                                    if slot.Named then
                                        match data.TryFind slot with
                                        | Some value ->
                                            match value with
                                            | Value.Identifier identifier -> identifier
                                            | _ -> failwith "Error"
                                        | None -> failwith "Error"
                                    else
                                        failwith "Error"

                            let value =
                                match statement.Value with
                                | PatternValue.Value(v) -> v
                                | PatternValue.Slot(slot) ->
                                    if slot.Named then
                                        match data.TryFind slot with
                                        | Some value -> value
                                        | None -> failwith "Error"
                                    else
                                        failwith "Error"

                            { Entity = entity
                              Attribute = attribute
                              Value = value })
                    patternStatements

            Some(InMemoryDataset(res))

        member _.Dataset: IDataset option =
            let res: Set<Statement> =
                Set.map
                    (fun (statement: PatternStatement) ->
                        match statement with
                        | { Entity = PatternIdentifier.Identifier(e)
                            Attribute = PatternIdentifier.Identifier(a)
                            Value = PatternValue.Value(v) } ->
                            { Entity = e; Attribute = a; Value = v }
                        | _ ->
                            let entity =
                                match statement.Entity with
                                | PatternIdentifier.Identifier identifier -> identifier
                                | PatternIdentifier.Slot _ -> failwith "Error"

                            let attribute =
                                match statement.Attribute with
                                | PatternIdentifier.Identifier identifier -> identifier
                                | PatternIdentifier.Slot _ -> failwith "TODO"

                            let value =
                                match statement.Value with
                                | PatternValue.Value v -> v
                                | PatternValue.Slot _ -> failwith "Error"
                            { Entity = entity; Attribute = attribute; Value = value }
                    )
                    patternStatements

            Some(InMemoryDataset(res))

let emptyPattern = InMemoryPattern(Set.empty)
