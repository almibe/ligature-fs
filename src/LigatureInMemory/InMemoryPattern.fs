// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.InMemory.Pattern

open Ligature
open Dataset

type InMemoryPattern(patternStatements: Set<PatternStatement>) =
    override _.Equals(other) =
        match other with
        | :? IPattern as other -> patternStatements.Equals(other.PatternStatements)
        | _ -> failwith "TODO"

    interface IPattern with
        member _.PatternStatements = patternStatements
        member _.CompareTo(other) = failwith "Not Implemented"
        // match other with
        // | :? InMemoryPattern as other -> statements.CompareTo(other.Statements)
        // | _ -> failwith "TODO"
        member _.Apply(data: Map<Slot, Value>) = failwith "TODO"
        member this.Dataset: IDataset option = failwith "Not Implemented"
// InMemoryDataset(
//     Set.map
//         (fun (statement: PatternStatement) ->
//             match statement with
//             | { Entity = PatternIdentifier.Identifier(_)
//                 Attribute = PatternIdentifier.Identifier(_)
//                 Value = PatternValue.Value(_) } -> statement
//             | _ ->
//                 let entity =
//                     match statement.Entity with
//                     | PatternIdentifier.Identifier(identifier) -> PatternIdentifier.Identifier(identifier)
//                     | PatternIdentifier.Slot(slot) ->
//                         if slot.Named then
//                             match data.TryFind slot with
//                             | Some value ->
//                                 match value with
//                                 | PatternValue.Value(Value.Identifier identifier) ->
//                                     PatternIdentifier.Identifier identifier
//                                 | _ -> failwith "Error"
//                             | None -> failwith "Error"
//                         else
//                             failwith "Error"

//                 let attribute =
//                     match statement.Attribute with
//                     | PatternIdentifier.Identifier(identifier) -> PatternIdentifier.Identifier(identifier)
//                     | PatternIdentifier.Slot(slot) ->
//                         if slot.Named then
//                             match data.TryFind slot with
//                             | Some value ->
//                                 match value with
//                                 | PatternValue.Value(Value.Identifier identifier) ->
//                                     PatternIdentifier.Identifier identifier
//                                 | _ -> failwith "Error"
//                             | None -> failwith "Error"
//                         else
//                             failwith "Error"

//                 let value =
//                     match statement.Value with
//                     | PatternValue.Value(v) -> PatternValue.Value(v)
//                     | PatternValue.Slot(slot) ->
//                         if slot.Named then
//                             match data.TryFind slot with
//                             | Some value -> value
//                             | None -> failwith "Error"
//                         else

//                             failwith "Error"

//                 { Entity = entity
//                   Attribute = attribute
//                   Value = value })
//         statements
// )

// member _.Extract(data: IPattern) : Map<Slot, PatternValue> array =
//     if statements.IsEmpty || data.Statements.IsEmpty then
//         [||]
//     else
//         let mutable res = [||]

//         if Set.isSubset statements data.Statements then //TODO make sure this pattern is a dataset
//             res <- [| Map.empty |]
//         else
//             let mutable canidate = data.Statements

//             Set.iter
//                 (fun (pattern: PatternStatement) ->
//                     let entity =
//                         match pattern.Entity with
//                         | PatternIdentifier.Identifier identifier ->
//                             canidate <-
//                                 Set.filter
//                                     (fun statement ->
//                                         match statement.Entity with
//                                         | PatternIdentifier.Identifier id2 -> identifier = id2
//                                         | PatternIdentifier.Slot slot -> false)
//                                     canidate
//                         | PatternIdentifier.Slot slot ->
//                             if slot.Named then
//                                 failwith "TODO" //do nothing for now
//                             else
//                                 () //do nothing for now

//                     let attribute =
//                         match pattern.Attribute with
//                         | PatternIdentifier.Identifier identifier ->
//                             canidate <-
//                                 Set.filter
//                                     (fun statement ->
//                                         match statement.Attribute with
//                                         | PatternIdentifier.Identifier id2 -> identifier = id2
//                                         | PatternIdentifier.Slot slot -> false)
//                                     canidate
//                         | PatternIdentifier.Slot slot -> () //do nothing for now

//                     let value =
//                         match pattern.Value with
//                         | PatternValue.Value value ->
//                             canidate <-
//                                 Set.filter
//                                     (fun statement ->
//                                         match statement.Value with
//                                         | PatternValue.Value v2 -> value = v2
//                                         | PatternValue.Slot slot -> false)
//                                     canidate
//                         | PatternValue.Slot slot -> () //do nothing for now

//                     if not canidate.IsEmpty then
//                         res <- Array.append res [| Map.empty |])
//                 data.Statements

//         res

let emptyPattern = InMemoryPattern(Set.empty)
