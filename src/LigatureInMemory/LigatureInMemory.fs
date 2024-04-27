// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.InMemory

open Ligature
open Ligature.Wander.InMemoryDataset

type LigatureInMemory() =
    let datasets: Map<DatasetName, Set<Statement>> ref = ref Map.empty
    let mutable isOpen = true

    interface ILigature with
        member _.AllDatasets() =
            Ok(Map.keys datasets.Value |> List.ofSeq) //Ok datasets.Value

        member _.DatasetExists dataset =
            Map.containsKey dataset datasets.Value |> Ok

        member this.CreateDataset dataset =
            lock this (fun () ->
                if not (datasets.Value.ContainsKey dataset) then
                    datasets.Value <- Map.add dataset Set.empty datasets.Value //Set.add dataset datasets.Value

                Ok())

        member this.RemoveDataset dataset =
            lock this (fun () ->
                datasets.Value <- Map.remove dataset datasets.Value
                Ok())

        member _.AllStatements dataset =
            match Map.tryFind dataset datasets.Value with
            | Some(result) -> Ok(Set.toList result)
            | None -> failwith ""

        member _.AddStatements dataset statements =
            let contents = Map.find dataset datasets.Value
            let contents = Set.union contents (Set.ofSeq statements)
            datasets.Value <- Map.add dataset contents datasets.Value
            Ok()

        member _.RemoveStatements dataset statements =
            let contents = Map.find dataset datasets.Value
            let contents = Set.difference contents (Set.ofSeq statements)
            datasets.Value <- Map.add dataset contents datasets.Value
            Ok()

        member this.Close() =
            lock this (fun () ->
                isOpen <- false
                datasets.Value <- Map.empty
                Ok())

        member this.Call
            (dataset: DatasetName)
            (name: Identifier)
            (argument: IDataset)
            : Result<IDataset, LigatureError> =
            failwith "TODO"

        member this.Count (datasetName: DatasetName) (pattern: IPattern) : Result<int64, LigatureError> =
            failwith "TODO"

        member this.Contains (arg1: DatasetName) (arg2: IPattern) : Result<bool, LigatureError> =
            failwith "Not Implemented"
