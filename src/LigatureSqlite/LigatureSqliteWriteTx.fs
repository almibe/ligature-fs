// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Sqlite.WriteTx

open Ligature
open System
open Donald

let inline todo<'T> : 'T = raise (NotImplementedException("todo"))

type LigatureSqliteWriteTx(dataset: Dataset, datasetId: int64, conn, tx) =




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

