// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.InMemory.Dataset

open Ligature

type Store =
    { Store: Set<byte array>
      Strings: Map<string, byte array>
      Identifiers: Map<Identifier, byte array>
      Ints: Map<bigint, byte array>
      Bytes: Map<byte array, byte array> }

let emptyStore =
    { Store = Set.empty
      Strings = Map.empty
      Identifiers = Map.empty
      Ints = Map.empty
      Bytes = Map.empty }

type InMemoryDataset(statements: Set<Statement>) =
    let store =
        do
            Set.fold
                (fun store patternStatement ->

                    store)
                emptyStore
                statements

    // override _.Equals(other) =
    //     match other with
    //     | :? IDataset as other -> statements.Equals(other.Statements)
    //     | _ -> failwith "TODO"

    interface IDataset with
        //member _.AllStatements(_: unit) = failwith "TODO" //statements

        member _.Extract(data: IPattern) : Map<Slot, Value> array = failwith "TODO"
        member this.AllStatements() : Result<Statement list, LigatureError> = failwith "Not Implemented"
        member this.Contains(arg1: IPattern) : Result<bool, LigatureError> = failwith "Not Implemented"
        member this.Count(arg1: IPattern) : Result<int64, LigatureError> = failwith "Not Implemented"
// if statements.IsEmpty || data.Statements.IsEmpty then
//     [||]
// else
//     let mutable res = [||]

//     if Set.isSubset statements data.Statements then //TODO make sure this pattern is a dataset
//         res <- [| Map.empty |]
//     else
//         let mutable canidate = data.Statements

//         Set.iter
//             (fun (pattern: PatternStatement) ->
//                 let entity =
//                     match pattern.Entity with
//                     | PatternIdentifier.Identifier identifier ->
//                         canidate <-
//                             Set.filter
//                                 (fun statement ->
//                                     match statement.Entity with
//                                     | PatternIdentifier.Identifier id2 -> identifier = id2
//                                     | PatternIdentifier.Slot slot -> false)
//                                 canidate
//                     | PatternIdentifier.Slot slot ->
//                         if slot.Named then
//                             failwith "TODO" //do nothing for now
//                         else
//                             () //do nothing for now

//                 let attribute =
//                     match pattern.Attribute with
//                     | PatternIdentifier.Identifier identifier ->
//                         canidate <-
//                             Set.filter
//                                 (fun statement ->
//                                     match statement.Attribute with
//                                     | PatternIdentifier.Identifier id2 -> identifier = id2
//                                     | PatternIdentifier.Slot slot -> false)
//                                 canidate
//                     | PatternIdentifier.Slot slot -> () //do nothing for now

//                 let value =
//                     match pattern.Value with
//                     | PatternValue.Value value ->
//                         canidate <-
//                             Set.filter
//                                 (fun statement ->
//                                     match statement.Value with
//                                     | PatternValue.Value v2 -> value = v2
//                                     | PatternValue.Slot slot -> false)
//                                 canidate
//                     | PatternValue.Slot slot -> () //do nothing for now

//                 if not canidate.IsEmpty then
//                     res <- Array.append res [| Map.empty |])
//             data.Statements

//     res

let emptyDataset = InMemoryDataset(Set.empty)
