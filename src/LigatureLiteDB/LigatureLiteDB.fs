// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.LiteDB.Main

open Ligature
open Ligature.LiteDB.QueryTx
open System.Data
open FsToolkit.ErrorHandling

type LigatureLiteDB(conn: string) = //(datasource: string) =
    interface ILigature with
        member _.AllDatasets() = failwith "TODO"
        member _.DatasetExists (Dataset dataset) = failwith "TODO"

        member _.CreateDataset (Dataset dataset) = failwith "TODO"

        member this.RemoveDataset (Dataset dataset) = failwith "TODO"

        member _.AllStatements dataset = failwith "TODO"

        member _.NewIdentifier dataset : Result<Identifier, LigatureError> = failwith "todo"//Guid.NewGuid().ToString() |> identifier

        member _.AddStatements (dataset: Dataset) (statements: Statement list) = failwith "TODO"

        member _.RemoveStatements (dataset: Dataset) (statements: Statement list) : Result<unit, LigatureError> = failwith "TODO"

        member _.Query dataset query = failwith "TODO"

        // member _.Write dataset write =
        //     let dbTx = conn.TryBeginTransaction()
        //     let datasetId = lookupDataset dataset dbTx
        //     match datasetId with
        //     | Ok(id) ->
        //         let tx = new LigatureSqliteWriteTx(dataset, id, conn, dbTx)
        //         match write tx with
        //         | Ok _ ->
        //             dbTx.Commit()
        //             Ok()
        //         | Error(err) -> Error(err)
        //     | Error(err) -> Error(err)

        member _.Close() = failwith "TODO"

let ligatureLiteDB (config: string) : ILigature = failwith "TODO"
