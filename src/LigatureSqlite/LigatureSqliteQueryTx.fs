// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Sqlite.QueryTx

open Ligature
open System.Data
open Donald

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

let statementDataReader (rd : IDataReader) : string * string =
    (rd.ReadString "name", "")

let makeStatement (input: string * string): Result<Statement, LigatureError> = todo

let rec makeStatements (input: (string * string) list) (statements: Statement array): Result<Statement array, LigatureError> =
    if List.isEmpty input then
        Ok (statements)
    else
        match makeStatement (List.head input) with
        | Ok(statement) -> makeStatements (List.tail input) (Array.append statements [|statement|])
        | Error(error) -> Error(error)

type LigatureSqliteQueryTx(dataset: Dataset, conn, tx) =
    interface QueryTx with
        member _.AllStatements () =
            let sql = "select * from dataset inner join statement on dataset.rowid = statement.dataset where dataset.name = @name"
            let param = [ "name", SqlType.String (readDataset dataset) ]
            let results = 
                conn
                |> Db.newCommand sql
                |> Db.setParams param
                |> Db.setTransaction tx
                |> Db.query statementDataReader
            match results with
            | Ok(results) -> makeStatements results Array.empty //Ok (List.toArray results)
            | Error(_) -> error "Oops" None
        member _.MatchStatements entity attribute value =
            todo
