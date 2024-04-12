// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.InMemory

open Ligature
open System.IO
open Ligature.Wander.Model
open Ligature.Wander.Main
open Ligature.Wander.Bindings

type LigatureInMemoryQueryTx(statements: Set<Statement>) =
    interface IDataset with
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
            failwith "TODO"
        member this.AllStatements(): Result<Statement list,LigatureError> = 
            failwith "Not Implemented"
            //Ok (List.ofSeq results)

type LigatureInMemory() =
    let datasets: Map<DatasetName, Set<Statement>> ref = ref Map.empty
    let mutable isOpen = true

    member _.Write(writer: TextWriter) =
        Map.iter (fun (DatasetName dataset) statements -> 
            writer.Write (prettyPrint (WanderValue.String dataset))
            writer.WriteLine ()
            Set.iter 
                (fun statement -> 
                    writer.Write (printStatement statement)
                    writer.WriteLine ()) 
                statements) datasets.Value

    member this.LoadFromString(content: string seq) =
        let mutable dataset = None
        let instance: ILigature = this
        content
        |> Seq.iter (fun row -> 
            match run row (newBindings ()) with
            | Ok(WanderValue.String(datasetName)) ->
                instance.CreateDataset (DatasetName datasetName) |> ignore
                dataset <- Some (DatasetName datasetName)
            | Ok(WanderValue.Statement(statement)) ->
                match dataset with
                | Some dataset -> instance.AddStatements dataset [statement] |> ignore
                | _ -> failwith "Expected Dataset to be set"
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
            failwith "TODO"
//            query tx

        // member _.AllStatements dataset = 
        //     match Map.tryFind dataset datasets.Value with
        //     | Some(result) -> Ok(Set.toList result)
        //     | None -> failwith ""

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
