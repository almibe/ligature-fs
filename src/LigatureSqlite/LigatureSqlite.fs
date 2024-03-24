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

type LigatureSqlite(conn: SQLiteConnection) = //(datasource: string) =
    let datasetDataReader (rd: IDataReader) : string = rd.ReadString "name"

    let lookupDataset (Dataset dataset) tx : Result<int64, LigatureError> =
        let sql = "select rowid, name from dataset where name = @name"
        let param = [ "name", SqlType.String(dataset) ]

        let results =
            conn
            |> Db.newCommand sql
            |> Db.setTransaction tx
            |> Db.setParams param
            |> Db.query (fun x -> x.ReadInt64 "rowid", x.ReadString "name") //TODO use Db.read not Db.query

        match results with
        | Ok([ result ]) -> Ok(fst result)
        | _ -> error $"Could not lookup Dataset {dataset}" None

    member _.initialize() =
        let sql =
            "
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

        conn |> Db.newCommand sql |> Db.exec

    interface ILigature with
        member _.AllDatasets() =
            let sql = "select * from dataset"
            let results = conn |> Db.newCommand sql |> Db.query datasetDataReader

            match results with
            | Ok(results) -> results |> List.map (fun s -> Dataset s) |> Ok //Ok (List.toArray results)
            | Error(_) -> error "Could not read all Datasets." None

        member _.DatasetExists (Dataset dataset) =
            let sql = "select name from dataset where name = @name"
            let param = [ "name", SqlType.String(dataset) ]

            let results =
                conn
                |> Db.newCommand sql
                |> Db.setParams param
                |> Db.query (fun x -> x.ReadString "name")

            match results with
            | Ok(results) -> results.Length > 0 |> Ok
            | Error(_) -> Ok false

        member this.CreateDataset (Dataset dataset) =
            let instance = this :> ILigature

            match instance.DatasetExists (Dataset dataset) with
            | Ok(true) -> Ok()
            | Ok(false) ->
                let sql = "insert into dataset (name) values (@name)"
                let param = [ "name", SqlType.String(dataset) ]
                let results = conn |> Db.newCommand sql |> Db.setParams param |> Db.exec

                match results with
                | Ok(_) -> Ok()
                | Error(_) -> error $"Could not create dataset {dataset}" None
            | Error(error) -> Error(error)

        member this.RemoveDataset (Dataset dataset) =
            let instance = this :> ILigature

            match instance.DatasetExists (Dataset dataset) with
            | Ok(true) ->
                let sql = "delete from dataset where name = @name"
                let param = [ "name", SqlType.String(dataset) ]
                let results = conn |> Db.newCommand sql |> Db.setParams param |> Db.exec

                match results with
                | Ok(_) -> Ok()
                | Error(_) -> error $"Could not delete dataset {dataset}" None
            | Ok(false) -> Ok()
            | Error(error) -> Error(error)

        member _.Query dataset query =
            let dbTx = conn.TryBeginTransaction()
            let datasetId = lookupDataset dataset dbTx

            match datasetId with
            | Ok(id) ->
                let tx = new LigatureSqliteQueryTx(dataset, id, conn, dbTx)
                let res = query tx
                dbTx.Rollback()
                res
            | Error(err) -> Error(err)

        member _.Write dataset write =
            let dbTx = conn.TryBeginTransaction()
            let datasetId = lookupDataset dataset dbTx
            match datasetId with
            | Ok(id) ->
                let tx = new LigatureSqliteWriteTx(dataset, id, conn, dbTx)
                match write tx with
                | Ok _ ->
                    dbTx.Commit()
                    Ok()
                | Error(err) -> Error(err)
            | Error(err) -> Error(err)

        member _.Close() =
            conn.Close()
            Ok()

type LigatureSqliteConfig =
    | InMemory
    | File of string

let ligatureSqlite (config: LigatureSqliteConfig) : ILigature =
    let instance =
        match config with
        | InMemory -> new LigatureSqlite(new SQLiteConnection("Data Source=:memory:"))
        | File(path) ->
            let builder = new SQLiteConnectionStringBuilder()
            builder.DataSource <- path
            new LigatureSqlite(new SQLiteConnection(builder.ToString()))

    instance.initialize () |> ignore
    instance
