// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.InMemory

open Ligature

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

type LigatureInMemoryQueryTx(statements: Set<Edge>) =
    interface IQueryTx with
        member _.AllEdges() = Ok(List.ofSeq statements)

        member _.MatchEdges entity attribute value =
            let results =
                match entity with
                | Some(entity) -> Set.filter (fun statement -> statement.Source = entity) statements
                | None -> statements

            let results =
                match attribute with
                | Some(attribute) -> Set.filter (fun statement -> statement.Label = attribute) results
                | None -> results

            let results =
                match value with
                | Some(value) -> Set.filter (fun statement -> statement.Target = value) results
                | None -> results

            List.ofSeq results |> Ok

type LigatureInMemoryWriteTx(dataset: Graph, datasets: Map<Graph, Set<Edge>> ref) =
    interface IWriteTx with
        member _.NewLabel() = todo

        member _.AddEdge statement =
            let statements = Map.find dataset datasets.Value
            let statements = Set.add statement statements
            datasets.Value <- Map.add dataset statements datasets.Value
            Ok()

        member _.RemoveEdge statement =
            let statements = Map.find dataset datasets.Value
            let statements = Set.remove statement statements
            datasets.Value <- Map.add dataset statements datasets.Value
            Ok()

type LigatureInMemory() =
    let datasets: Map<Graph, Set<Edge>> ref = ref Map.empty
    let mutable isOpen = true

    interface ILigature with
        member _.AllGraphs() =
            Ok(Map.keys datasets.Value |> Seq.cast |> List.ofSeq) //Ok datasets.Value

        member _.GraphExists dataset =
            Map.containsKey dataset datasets.Value |> Ok

        member this.CreateGraph dataset =
            lock this (fun () ->
                if not (datasets.Value.ContainsKey dataset) then
                    datasets.Value <- Map.add dataset Set.empty datasets.Value //Set.add dataset datasets.Value
                Ok())

        member this.RemoveGraph dataset =
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
