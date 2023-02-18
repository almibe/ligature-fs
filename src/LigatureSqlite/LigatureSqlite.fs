// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Sqlite.Main

open Ligature
open Ligature.Sqlite.QueryTx
open Ligature.Sqlite.WriteTx
open System.Data.SQLite
open System.Data
open Donald

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

let rec makeDatasets (names: string list) (datasets: Dataset array): Result<Dataset array, LigatureError> =
    if List.isEmpty names then
        Ok (datasets)
    else
        match dataset (List.head names) with
        | Ok(dataset) -> makeDatasets (List.tail names) (Array.append datasets [|dataset|])
        | Error(error) -> Error(error)

type LigatureSqlite() =
    let datasetDataReader (rd : IDataReader) : string = rd.ReadString "name"
    let conn = new SQLiteConnection("Data Source=:memory:")
    member this.initialize () =
        let sql = "
        create table if not exists dataset (
        name String not null
        );

        create table if not exists identifier (
        identifier String not null
        );

        create table if not exists statement (
        dataset int not null,
        entity int not null,
        attribute int not null,
        value_identifier int,
        value_string String,
        value_integer int
        );"

        conn
        |> Db.newCommand sql
        |> Db.exec

    interface Ligature with
        member _.AllDatasets ()  =
            let sql = "select * from dataset"
            let results = 
                conn
                |> Db.newCommand sql
                |> Db.query datasetDataReader
            match results with
            | Ok(results) -> makeDatasets results Array.empty //Ok (List.toArray results)
            | Error(_) -> error "Oops" None

        member _.DatasetExists dataset =
            let sql = "select name from dataset where name = @name"
            let param = [ "name", SqlType.String (readDataset dataset) ]
            let results =
                conn
                |> Db.newCommand sql
                |> Db.setParams param
                |> Db.query (fun x -> x.ReadString "name")
            match results with
            | Ok(results) -> results.Length > 0 |> Ok
            | Error(_) -> Ok false
        
        member this.CreateDataset dataset =
            let instance = this :> Ligature
            match instance.DatasetExists dataset with
            | Ok(true) -> todo
            | Ok(false) ->
                let sql = "insert into dataset (name) values (@name)"
                let param = [ "name", SqlType.String (readDataset dataset) ]
                let results =
                    conn
                    |> Db.newCommand sql
                    |> Db.setParams param
                    |> Db.exec
                match results with
                | Ok(_) -> Ok ()
                | Error(_) -> error $"Could not create dataset {dataset}" None
            | Error(error) -> Error(error)
        
        member this.RemoveDataset dataset =
            let instance = this :> Ligature
            match instance.DatasetExists dataset with
            | Ok(true) ->
                let sql = "delete from dataset where name = @name"
                let param = [ "name", SqlType.String (readDataset dataset) ]
                let results =
                    conn
                    |> Db.newCommand sql
                    |> Db.setParams param
                    |> Db.exec
                match results with
                | Ok(_) -> Ok ()
                | Error(_) -> error $"Could not delete dataset {dataset}" None                
            | Ok(false) -> Ok ()
            | Error(error) -> Error(error)
        
        member this.Query dataset query =
            let dbTx = conn.TryBeginTransaction()
            let tx = new LigatureSqliteQueryTx(dataset, conn, dbTx)
            query tx
        
        member this.Write dataset write =
            let dbTx = conn.TryBeginTransaction()
            let tx = new LigatureSqliteWriteTx(dataset, conn, dbTx)
            write tx
        
        member this.Close () =
            conn.Close()
            Ok ()
