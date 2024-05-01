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
        member this.Extract(pattern: IPattern) : Map<Slot, Value> list =
            if statements.IsEmpty || pattern.PatternStatements.IsEmpty then
                List.Empty
            else
                let mutable res: Map<Slot, Value> list = List.empty //TODO make a list
                let mutable currentNames: Map<Slot, Value> = Map.empty

                statements
                |> Set.iter (fun statement ->                    
                    currentNames <- Map.empty //reset state
                    pattern.PatternStatements
                    |> Set.iter (fun (pattern: PatternStatement) ->
                        let mutable matched = true

                        match pattern.Entity with
                        | PatternIdentifier.Identifier identifier -> matched <- statement.Entity = identifier
                        | PatternIdentifier.Slot slot ->
                            if slot.Named then
                                if currentNames.ContainsKey slot then
                                    failwith "TODO"
                                else
                                    currentNames <- currentNames.Add(slot, Value.Identifier statement.Entity)
                            else
                                ()

                        match pattern.Attribute with
                        | PatternIdentifier.Identifier identifier -> matched <- statement.Attribute = identifier
                        | PatternIdentifier.Slot slot ->
                            if slot.Named then
                                if currentNames.ContainsKey slot then
                                    failwith "TODO"
                                else
                                    currentNames <- currentNames.Add(slot, Value.Identifier statement.Attribute)
                            else
                                ()

                        match pattern.Value with
                        | PatternValue.Value value -> matched <- statement.Value = value
                        | PatternValue.Slot slot ->
                            if slot.Named then
                                if currentNames.ContainsKey slot then
                                    failwith "TODO"
                                else
                                    currentNames <- currentNames.Add(slot, statement.Value)
                            else
                                ()

                        if matched then res <- List.append res [ currentNames ]))
                List.ofSeq res

        member _.AllStatements() : Result<Statement list, LigatureError> = Ok(List.ofSeq statements)
        member this.Contains(pattern: IPattern) : Result<bool, LigatureError> = 
            let this = this :> IDataset
            Ok(this.Extract(pattern).IsEmpty)
        member this.Count(pattern: IPattern) : Result<int64, LigatureError> = 
            let this = this :> IDataset
            Ok(this.Extract(pattern).Length)

let emptyDataset = InMemoryDataset(Set.empty)
