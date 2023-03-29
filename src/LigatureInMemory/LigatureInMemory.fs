// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.InMemory

open Ligature

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

type LigatureInMemoryQueryTx(statements: Set<Statement>) =
    interface IQueryTx with
        member _.AllStatements() = Ok(List.ofSeq statements)

        member _.MatchStatements entity attribute value =
            let results =
                match entity with
                | Some(entity) -> Set.filter (fun statement -> statement.Entity = entity) statements
                | None -> statements

            let results =
                match attribute with
                | Some(attribute) -> Set.filter (fun statement -> statement.Attribute = attribute) results
                | None -> results

            let results =
                match value with
                | Some(value) -> Set.filter (fun statement -> statement.Value = value) results
                | None -> results

            List.ofSeq results |> Ok

type LigatureInMemoryWriteTx(dataset: Dataset, datasets: Map<Dataset, Set<Statement>> ref) =
    interface IWriteTx with
        member _.NewIdentifier() = todo

        member _.AddStatement statement =
            let statements = Map.find dataset datasets.Value
            let statements = Set.add statement statements
            datasets.Value <- Map.add dataset statements datasets.Value
            Ok()

        member _.RemoveStatement statement =
            let statements = Map.find dataset datasets.Value
            let statements = Set.remove statement statements
            datasets.Value <- Map.add dataset statements datasets.Value
            Ok()

type LigatureInMemory() =
    let datasets: Map<Dataset, Set<Statement>> ref = ref Map.empty
    let mutable isOpen = true

    interface ILigature with
        member _.AllDatasets() =
            Ok(Map.keys datasets.Value |> Seq.cast |> List.ofSeq) //Ok datasets.Value

        member _.DatasetExists dataset =
            Map.containsKey dataset datasets.Value |> Ok

        member this.CreateDataset dataset =
            lock this (fun () ->
                datasets.Value <- Map.add dataset Set.empty datasets.Value //Set.add dataset datasets.Value
                Ok())

        member this.RemoveDataset dataset =
            lock this (fun () ->
                datasets.Value <- Map.remove dataset datasets.Value
                Ok())

        member _.Query dataset query =
            let tx = new LigatureInMemoryQueryTx(Map.find dataset datasets.Value)
            query tx

        member this.Write dataset write =
            lock this (fun () ->
                let tx = new LigatureInMemoryWriteTx(dataset, datasets)
                write tx)

        member this.Close() =
            lock this (fun () ->
                isOpen <- false
                datasets.Value <- Map.empty
                Ok())
