// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Pattern

open Ligature

type PatternSet(statements: Set<PatternStatement>) =
    interface IPattern with
        member _.Statements = statements
        member _.CompareTo(other) =
            failwith "Not Implemented"
            // match other with
            // | :? PatternSet as other -> statements.CompareTo(other.Statements)
            // | _ -> failwith "TODO"
        member _.Apply (data: Map<Slot, PatternValue>) = 
            PatternSet(
                                Set.map
                                    (fun (statement: PatternStatement) ->
                                        match statement with
                                        | { Entity = PatternIdentifier.Identifier(_)
                                            Attribute = PatternIdentifier.Identifier(_)
                                            Value = PatternValue.Value(_) } -> statement
                                        | _ ->
                                            let entity =
                                                match statement.Entity with
                                                | PatternIdentifier.Identifier(identifier) ->
                                                    PatternIdentifier.Identifier(identifier)
                                                | PatternIdentifier.Slot(slot) ->
                                                    if slot.Named then
                                                        match data.TryFind slot with
                                                        | Some value ->
                                                            match value with
                                                            | PatternValue.Value(Value.Identifier identifier) ->
                                                                PatternIdentifier.Identifier identifier
                                                            | _ -> failwith "Error"
                                                        | None -> failwith "Error"
                                                    else
                                                        failwith "Error"

                                            let attribute =
                                                match statement.Attribute with
                                                | PatternIdentifier.Identifier(identifier) ->
                                                    PatternIdentifier.Identifier(identifier)
                                                | PatternIdentifier.Slot(slot) -> failwith "TODO"

                                            let value =
                                                match statement.Value with
                                                | PatternValue.Value(v) -> PatternValue.Value(v)
                                                | PatternValue.Slot(slot) -> failwith "TODO"

                                            { Entity = entity
                                              Attribute = attribute
                                              Value = value })
                                    statements
                            )
         //   failwith "Not Implemented"
        member this.Extract(arg1: IPattern) : Map<Slot, PatternValue> = failwith "Not Implemented"

let emptyPattern = PatternSet(Set.empty)
