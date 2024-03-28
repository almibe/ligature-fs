// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Sqlite.WriteTx

open Ligature
open System
open Donald

let inline todo<'T> : 'T = raise (NotImplementedException("todo"))

type LigatureSqliteWriteTx(dataset: Dataset, datasetId: int64, conn, tx) =
    let (Dataset datasetName) = dataset
    let getLastRowId () : Result<int64, LigatureError> =
        let sql = "select last_insert_rowid()"

        let result =
            conn |> Db.newCommand sql |> Db.setTransaction tx |> Db.scalar Convert.ToInt64

        Result.mapError (fun err -> todo) result

    let createIdentifier (identifier: Identifier) : Result<int64, LigatureError> =
        let sql = "insert into identifier (identifier) values (@identifier)"
        let param = [ "identifier", SqlType.String(readIdentifier identifier) ]

        let results =
            conn
            |> Db.newCommand sql
            |> Db.setTransaction tx
            |> Db.setParams param
            |> Db.exec

        match results with
        | Ok(_) -> getLastRowId ()
        | Error(_) -> error $"Could not create dataset {dataset}" None

    let checkIdentifier (identifier: Identifier) : Result<int64, LigatureError> =
        let sql = "select *, rowid from identifier where identifier = @identifier"
        let param = [ "identifier", SqlType.String(readIdentifier identifier) ]

        let results =
            conn
            |> Db.newCommand sql
            |> Db.setTransaction tx
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
                    [ "value_identifier", SqlType.Int64 id
                      "value_string", SqlType.Null
                      "value_integer", SqlType.Null ]
            | Error(err) -> Error(err)
        | Value.String(value) ->
            Ok
                [ "value_identifier", SqlType.Null
                  "value_string", SqlType.String value
                  "value_integer", SqlType.Null ]
        | Value.Integer(value) ->
            Ok
                [ "value_identifier", SqlType.Null
                  "value_string", SqlType.Null
                  "value_integer", SqlType.Int64 value ]

    let statementExists
        datasetId
        entityId
        attributeId
        (valueParam: (string * SqlType) list)
        : Result<bool, LigatureError> =
        let sql =
            "select count(*) from statement where
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
            |> Db.setTransaction tx
            |> Db.setParams param
            |> Db.scalar Convert.ToInt64

        let result = Result.map (fun res -> res > 0L) result

        Result.mapError
            (fun err ->
                { UserMessage = "Could not insert Statement."
                  DebugMessage = (Some $"DB Error - {err}") })
            result

    let insertStatement
        datasetId
        entityId
        attributeId
        (valueParam: (string * SqlType) list)
        : Result<unit, LigatureError> =
        let sql =
            "insert into statement
            (dataset, entity, attribute, value_identifier, value_string, value_integer)
            values (@dataset, @entity, @attribute, @value_identifier, @value_string, @value_integer)"

        let param =
            List.append
                [ "dataset", SqlType.Int64 datasetId
                  "entity", SqlType.Int64 entityId
                  "attribute", SqlType.Int64 attributeId ]
                valueParam

        let result =
            conn
            |> Db.newCommand sql
            |> Db.setTransaction tx
            |> Db.setParams param
            |> Db.exec

        Result.mapError
            (fun err ->
                { UserMessage = "Could not insert Statement."
                  DebugMessage = (Some $"DB Error - {err}") })
            result

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
            |> Db.setTransaction tx
            |> Db.setParams param
            |> Db.exec

        Result.mapError
            (fun err ->
                { UserMessage = $"Could not remove Statement."
                  DebugMessage = (Some $"DB Error - {err}") })
            result
