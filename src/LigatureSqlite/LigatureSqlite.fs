// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Sqlite

open Ligature
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
        member this.AllDatasets ()  =
            let sql = "select * from dataset"
            let results = 
                conn
                |> Db.newCommand sql
                |> Db.query datasetDataReader
            match results with
            | Ok(results) -> makeDatasets results Array.empty //Ok (List.toArray results)
            | Error(_) -> error "Oops" None
        member this.DatasetExists dataset =
            let sql = "select name from dataset where name = @name"
            let param = [ "name", SqlType.String (readDataset dataset) ]
            let results =
                conn
                |> Db.newCommand sql
                |> Db.setParams param
                |> Db.query (fun x -> x.ReadString "name")
            todo
        member this.CreateDataset dataset =
            let instance = this :> Ligature
            if instance.DatasetExists dataset then
                todo
            else
                let sql = "insert into dataset (name) values (@name)"
                let param = [ "name", SqlType.String (readDataset dataset) ]
                let results =
                    conn
                    |> Db.newCommand sql
                    |> Db.setParams param
                    |> Db.query (fun x -> x.ReadString "name")
                match results with
                | Ok(results) -> todo
                | Error(error) -> todo
        member this.RemoveDataset dataset =
            let instance = this :> Ligature
            if instance.DatasetExists dataset then
                todo
            else
                todo
        member this.Query dataset query = todo
        member this.Write dataset write = todo
        member this.Close () =
            conn.Close()
            Ok ()
