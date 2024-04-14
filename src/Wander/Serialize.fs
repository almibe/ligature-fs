// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module rec Ligature.Wander.Serialize

open Lexer
open FsToolkit.ErrorHandling
open Model
open Nibblers
open Ligature
open System.Collections
open System.IO
open Main
open Bindings

let write (writer: TextWriter) (instance: ILigature) = failwith ""
// Map.iter
//     (fun (DatasetName dataset) statements ->
//         writer.Write(prettyPrint (WanderValue.String dataset))
//         writer.WriteLine()

//         Set.iter
//             (fun statement ->
//                 writer.Write(printStatement statement)
//                 writer.WriteLine())
//             statements)
//     datasets.Value

let loadFromString (content: string seq) (instance: ILigature) =
    let mutable dataset = None

    content
    |> Seq.iter (fun row ->
        match run row (newBindings ()) with
        | Ok(WanderValue.String(datasetName)) ->
            instance.CreateDataset(DatasetName datasetName) |> ignore
            dataset <- Some(DatasetName datasetName)
        | Ok(WanderValue.Statement(statement)) ->
            match dataset with
            | Some dataset -> instance.AddStatements dataset [ statement ] |> ignore
            | _ -> failwith "Expected Dataset to be set"
        | _ -> ())
