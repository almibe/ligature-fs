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
open FsToolkit.ErrorHandling
open Ligature.Sqlite.WriteTx
open System

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

type LigatureSqlite(conn: SQLiteConnection) = //(datasource: string) =
    let datasetDataReader (rd: IDataReader) : string = rd.ReadString "name"

    let getLastRowId () : Result<int64, LigatureError> =
        let sql = "select last_insert_rowid()"

        let result =
            conn |> Db.newCommand sql |> Db.scalar Convert.ToInt64

        Result.mapError (fun err -> todo) result


    let createIdentifier (identifier: Identifier) : Result<int64, LigatureError> =
        let sql = "insert into identifier (identifier) values (@identifier)"
        let param = [ "identifier", SqlType.String(readIdentifier identifier) ]

        let results =
            conn
            |> Db.newCommand sql
            |> Db.setParams param
            |> Db.exec

        match results with
        | Ok(_) -> getLastRowId ()
        | Error(_) -> error $"Could not create Identifier {identifier}." None

    let checkIdentifier (identifier: Identifier) : Result<int64, LigatureError> =
        let sql = "select *, rowid from identifier where identifier = @identifier"
        let param = [ "identifier", SqlType.String(readIdentifier identifier) ]

        let results =
            conn
            |> Db.newCommand sql
            |> Db.setParams param
            |> Db.query (fun x -> x.ReadInt64 "rowid")

        match results with
        | Ok([]) -> createIdentifier identifier
        | Ok([ id ]) -> Ok(id)
        | Ok(_) -> todo
        | Error(err) -> error $"Error checking for identifier {(readIdentifier identifier)}." (Some $"DBError - {err}")

    let createValueParams (value: Value) : Result<(string * SqlType) list, LigatureError> =
        match value with
        | Value.Identifier(value) ->
            let id = checkIdentifier value

            match id with
            | Ok(id) ->
                Ok
                    [ ("value_identifier", SqlType.Int64 id);
                    ("value_string", SqlType.Null);
                    ("value_integer", SqlType.Null) ]
            | Error(err) -> Error(err)
        | Value.String(value) ->
            Ok
                [ ("value_identifier", SqlType.Null);
                ("value_string", SqlType.String value);
                ("value_integer", SqlType.Null) ]
        | Value.Integer(value) ->
            Ok
                [ ("value_identifier", SqlType.Null);
                ("value_string", SqlType.Null);
                ("value_integer", SqlType.Int64 value) ]

    let deleteStatement
        datasetId
        entityId
        attributeId
        (valueParam: (string * SqlType) list)
        : Result<unit, LigatureError> =
        let sql =
            "delete from statement where
            dataset = @dataset and
            entity = @entity and
            attribute = @attribute and
            "

        let valueQuery =
            valueParam
            |> List.filter (fun (_, t) -> t = SqlType.Null |> not)
            |> List.head
            |> fst
            |> fun v -> $"{v} = @{v}"

        let sql = sql + valueQuery

        let param =
            List.append
                [ "dataset", SqlType.Int64 datasetId
                  "entity", SqlType.Int64 entityId
                  "attribute", SqlType.Int64 attributeId ]
                valueParam

        let result =
            conn
            |> Db.newCommand sql
            |> Db.setParams param
            |> Db.exec

        Result.mapError
            (fun err ->
                { UserMessage = $"Could not remove Statement."
                  DebugMessage = (Some $"DB Error - {err}") })
            result

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

        member _.AllStatements dataset =
            let sql =
                "select Entity.identifier as entity, Attribute.identifier as attribute, Value.identifier as value_identifier,
                Statement.value_string as value_string, Statement.value_integer as value_integer from Dataset
                inner join Statement on Dataset.rowid = Statement.dataset 
                inner join Identifier as Entity on statement.entity = Entity.rowid 
                inner join Identifier as Attribute on Statement.attribute = Attribute.rowid 
                left join Identifier as Value on Statement.value_identifier = Value.rowid
                where dataset.name = @name"
            let (Dataset datasetName) = dataset
            let param = [ "name", SqlType.String(datasetName) ]
            let results =
                conn
                |> Db.newCommand sql
                |> Db.setParams param
                |> Db.query statementDataReader //TODO use Db.read not Db.query

            match results with
            | Ok(r) -> List.traverseResultM (fun r -> r) r
            | Error(dbError) -> error "Error reading Statements." (Some $"DB Error - {dbError}")

        member _.NewIdentifier dataset : Result<Identifier, LigatureError> = failwith "todo"//Guid.NewGuid().ToString() |> identifier

        member _.AddStatements (dataset: Dataset) (statements: Statement list) =
            failwith "todo"
            // let entityId = checkIdentifier statement.Entity
            // let attributeId = checkIdentifier statement.Attribute
            // let valueParams = createValueParams statement.Value

            // match (entityId, attributeId, valueParams) with
            // | (Ok(entityId), Ok(attributeId), Ok(valueParams)) ->
            //     match statementExists datasetId entityId attributeId valueParams with
            //     | Ok(true) -> Ok()
            //     | Ok(false) -> insertStatement datasetId entityId attributeId valueParams
            //     | Error(err) -> Error(err)
            // | _ -> todo

        member _.RemoveStatements (dataset: Dataset) (statements: Statement list) : Result<unit, LigatureError> =
            let dbTx = conn.TryBeginTransaction()
            let datasetId =  
                match lookupDataset dataset dbTx with
                | Ok id -> id
                | Error err -> failwith ""
            for statement in statements do
                let entityId = checkIdentifier statement.Entity
                let attributeId = checkIdentifier statement.Attribute
                let valueParams = createValueParams statement.Value

                match (entityId, attributeId, valueParams) with
                | (Ok(entityId), Ok(attributeId), Ok(valueParams)) ->
                    match deleteStatement datasetId entityId attributeId valueParams with
                    | Ok _ -> ()
                    | Error _ -> failwith "todo"
                | _ -> todo
            Ok ()

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
