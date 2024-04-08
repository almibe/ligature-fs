// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.LMDB.Main

open Ligature

let databases = [
    "Counters"
    "DatasetToId"
    "IdToDataset"
    "IdentifierToId"
    "IdToIdentifier"
    "StringToId"
    "IdToString"
    "BytesToId"
    "IdToBytes"
    "StatementDEAV"
    "StatementDEVA"
    "StatementDAEV"
    "StatementDAVE"
    "StatementDVEA"
    "StatementDVAE"
]

let nextId db name = System.Guid.NewGuid().ToByteArray()

type LigatureLMDB(path: string) = //(datasource: string) =
    let env = new LightningDB.LightningEnvironment(path, new LightningDB.EnvironmentConfiguration(MaxDatabases = 16))

    member _.openDb() = 
        env.Open()
        use tx = env.BeginTransaction()
        List.iter (fun databaseName ->
            tx.OpenDatabase(databaseName,
                new LightningDB.DatabaseConfiguration(Flags = LightningDB.DatabaseOpenFlags.Create))
            |> ignore
            ) databases
        tx.Commit()

    interface ILigature with
        member _.AllDatasets() =
            use tx = env.BeginTransaction()
            use db = tx.OpenDatabase("DatasetToId")
            use cursor = tx.CreateCursor(db)
            match cursor.First() with
            | LightningDB.MDBResultCode.NotFound -> Ok([])
            | _ ->
                let res = cursor.GetCurrent()
                match res with
                | (resultCode, key, value) ->
                    let datasetName = 

        member _.DatasetExists (Dataset dataset) =
            use tx = env.BeginTransaction()
            use db = tx.OpenDatabase("statements", 
                new LightningDB.DatabaseConfiguration(Flags = LightningDB.DatabaseOpenFlags.Create))
            use cursor = tx.CreateCursor(db)
            match cursor.First() with
            | LightningDB.MDBResultCode.NotFound -> Ok(false)
            | _ ->
                let res = cursor.GetCurrent()
                match res with
                | (resultCode, key, value) ->
                    printf "RCSuccess - %A" resultCode
                    failwith ""

        member _.CreateDataset (Dataset dataset) = 
            use tx = env.BeginTransaction()
            use datasetToIdDb = tx.OpenDatabase("DatasetToId")
            use idToDatasetDb = tx.OpenDatabase("IdToDataset")
            use countersDb = tx.OpenDatabase("Counters")
            let dataset = System.Text.Encoding.UTF32.GetBytes(dataset)
            match tx.Get(datasetToIdDb, dataset) with
            | (code, key, value) ->
                if code = LightningDB.MDBResultCode.Success then
                    Ok(())
                else
                    let datasetId = nextId countersDb "DatasetId"
                    tx.Put(datasetToIdDb, dataset, datasetId) |> ignore
                    tx.Put(idToDatasetDb, datasetId, dataset) |> ignore
                    tx.Commit () |> ignore
                    Ok(())

        member _.RemoveDataset (Dataset dataset) = failwith ""

        member _.AllStatements (Dataset dataset) = failwith ""

        member _.NewIdentifier dataset : Result<Identifier, LigatureError> = failwith "todo"//Guid.NewGuid().ToString() |> identifier

        member _.AddStatements (dataset: Dataset) (statements: Statement list) = failwith ""

        member _.RemoveStatements (dataset: Dataset) (statements: Statement list) : Result<unit, LigatureError> = failwith ""

        member _.Query dataset query = failwith "TODO"

        member _.Close() = failwith ""

let openLigatureLMDB location =
    let instance = new LigatureLMDB(location)
    instance.openDb ()
    instance

type LigatureSqliteQueryTx() =

    interface IQueryTx with
        member _.MatchStatements entity attribute value = failwith "TODO"
