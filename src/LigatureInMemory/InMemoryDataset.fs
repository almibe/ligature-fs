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
    // let store =
    //     do Set.fold (fun store patternStatement -> store)
    //         emptyStore
    //         statements

    interface IDataset with
        member _.Extract(pattern: IPattern) : Map<Slot, Value> list =
            if statements.IsEmpty || pattern.PatternStatements.IsEmpty then
                List.Empty
            else
                let mutable res: Map<Slot, Value> list = List.empty //TODO make a list
                let mutable canidate = pattern.PatternStatements
                let mutable currentNames: Map<Slot, Value> = Map.empty

                statements
                |> Set.iter (fun statement ->
                    pattern.PatternStatements
                    |> Set.iter
                        (fun (pattern: PatternStatement) ->
                            match pattern.Entity with
                            | PatternIdentifier.Identifier identifier ->
                                canidate <-
                                    Set.filter
                                        (fun statement ->
                                            match statement.Entity with
                                            | PatternIdentifier.Identifier id2 -> identifier = id2
                                            | PatternIdentifier.Slot slot -> false)
                                        canidate
                            | PatternIdentifier.Slot slot ->
                                if slot.Named then
                                    if currentNames.ContainsKey slot then
                                        failwith "TODO" //do nothing for now
                                    else
                                        currentNames <- currentNames.Add (slot, Value.Identifier statement.Entity)
                                else
                                    ()

                            match pattern.Attribute with
                            | PatternIdentifier.Identifier identifier ->
                                canidate <-
                                    Set.filter
                                        (fun statement ->
                                            match statement.Attribute with
                                            | PatternIdentifier.Identifier id2 -> identifier = id2
                                            | PatternIdentifier.Slot slot -> false)
                                        canidate
                            | PatternIdentifier.Slot slot ->
                                if slot.Named then
                                    failwith "TODO"
                                else
                                    ()

                            match pattern.Value with
                            | PatternValue.Value value ->
                                canidate <-
                                    Set.filter
                                        (fun statement ->
                                            match statement.Value with
                                            | PatternValue.Value v2 -> value = v2
                                            | PatternValue.Slot slot -> false)
                                        canidate
                            | PatternValue.Slot slot ->
                                if slot.Named then
                                    failwith "TODO"
                                else
                                    ()

                            if not canidate.IsEmpty then
                                res <- List.append res [ currentNames ]))
                List.ofSeq res

        member _.AllStatements() : Result<Statement list, LigatureError> = Ok(List.ofSeq statements)
        member _.Contains(pattern: IPattern) : Result<bool, LigatureError> = failwith "Not Implemented"
        member _.Count(pattern: IPattern) : Result<int64, LigatureError> = failwith "Not Implemented"

let emptyDataset = InMemoryDataset(Set.empty)
