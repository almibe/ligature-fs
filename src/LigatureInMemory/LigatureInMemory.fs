// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.InMemory

open Ligature
open System.IO
open Ligature.Bend.Model
open Ligature.Bend.Main
open Ligature.Bend.Bindings

type LigatureInMemoryQueryTx(statements: Set<Statement>) =
    interface IQueryTx with
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
            Ok (List.ofSeq results)

type LigatureInMemory() =
    let datasets: Map<Dataset, Set<Statement>> ref = ref Map.empty
    let mutable isOpen = true

    member _.Write(writer: TextWriter) =
        Map.iter (fun (Dataset dataset) statements -> 
            writer.Write (prettyPrint (BendValue.String dataset))
            writer.WriteLine ()
            Set.iter 
                (fun statement -> 
                    writer.Write (printStatement statement)
                    writer.WriteLine ()) 
                statements) datasets.Value

    member this.LoadFromString(content: string seq) =
        let mutable dataset = (Dataset (Seq.head content))
        Seq.tail content
        |> Seq.iter (fun row -> 
            match run row (newBindings ()) with
            | Ok(BendValue.String(datasetName)) ->
                let instance: ILigature = this
                instance.CreateDataset (Dataset datasetName) |> ignore
                dataset <- Dataset datasetName
            | Ok(BendValue.Statement(statement)) ->
                let instance: ILigature = this
                instance.AddStatements dataset [statement] |> ignore
            | _ -> ())

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

        member _.Query dataset query =
            let tx = new LigatureInMemoryQueryTx(Map.find dataset datasets.Value)
            query tx

        member _.AllStatements dataset = 
            match Map.tryFind dataset datasets.Value with
            | Some(result) -> Ok(Set.toList result)
            | None -> failwith ""

        member _.NewIdentifier data = failwith "todo"

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
