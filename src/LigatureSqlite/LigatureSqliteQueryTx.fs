// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Sqlite.QueryTx

open Ligature
open System.Data
open Donald
open FsToolkit.ErrorHandling

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

let readValue (rd: IDataReader) : Result<Value, LigatureError> =
    let valueIdentifier =
        rd.ReadStringOption "value_identifier" |> Option.map identifier

    let valueString = rd.ReadStringOption "value_string" |> Option.map String
    let valueInteger = rd.ReadInt64Option "value_integer" |> Option.map Integer

    match (valueIdentifier, valueString, valueInteger) with
    | (Some(i), _, _) -> Result.map Identifier i
    | (_, Some(s), _) -> Ok(s)
    | (_, _, Some(i)) -> Ok(i)
    | _ -> error "Could not read value." None

let statementDataReader (rd: IDataReader) : Result<Statement, LigatureError> =
    result {
        let! entity = rd.ReadString "entity" |> identifier
        let! attribute = rd.ReadString "attribute" |> identifier
        let! value = readValue rd

        return
            { Entity = entity
              Attribute = attribute
              Value = value }
    }

// let makeStatement (input: string * string): Result<Statement, LigatureError> = todo

// let rec makeStatements (input: (string * string) list) (statements: Statement array): Result<Statement array, LigatureError> =
//     if List.isEmpty input then
//         Ok (statements)
//     else
//         match makeStatement (List.head input) with
//         | Ok(statement) -> makeStatements (List.tail input) (Array.append statements [|statement|])
//         | Error(error) -> Error(error)

type LigatureSqliteQueryTx(dataset: Dataset, datasetId: int64, conn, tx) =
    let lookupIdentifier (identifier: Identifier) : Result<Option<int64>, LigatureError> =
        let sql = "select *, rowid from identifier where identifier = @identifier"
        let param = [ "identifier", SqlType.String(readIdentifier identifier) ]

        let results =
            conn
            |> Db.newCommand sql
            |> Db.setTransaction tx
            |> Db.setParams param
            |> Db.query (fun x -> x.ReadInt64 "rowid")

        match results with
        | Ok([]) -> Ok None
        | Ok([ id ]) -> Ok(Some id)
        | Ok(_) -> todo
        | Error(err) -> error $"Error checking for identifier {(readIdentifier identifier)}." (Some $"DBError - {err}")

    let createValueParams (value: Value) : Result<(string * SqlType) list, LigatureError> =
        match value with
        | Identifier(value) ->
            let id = lookupIdentifier value

            match id with
            | Ok(Some(id)) ->
                Ok
                    [ "value_identifier", SqlType.Int64 id
                      "value_string", SqlType.Null
                      "value_integer", SqlType.Null ]
            | Ok(None) -> todo
            | Error(err) -> Error(err)
        | String(value) ->
            Ok
                [ "value_identifier", SqlType.Null
                  "value_string", SqlType.String value
                  "value_integer", SqlType.Null ]
        | Integer(value) ->
            Ok
                [ "value_identifier", SqlType.Null
                  "value_string", SqlType.Null
                  "value_integer", SqlType.Int64 value ]

    let createValueQuery (valueParams: (string * SqlType) list) : string =
        valueParams
        |> List.filter (fun (_, t) -> t = SqlType.Null |> not)
        |> List.head
        |> fst
        |> fun v -> $"{v} = @{v}"

    interface IQueryTx with
        member _.AllStatements() =
            let sql =
                "select Entity.identifier as entity, Attribute.identifier as attribute, Value.identifier as value_identifier,
                Statement.value_string as value_string, Statement.value_integer as value_integer from Dataset
                inner join Statement on Dataset.rowid = Statement.dataset 
                inner join Identifier as Entity on statement.entity = Entity.rowid 
                inner join Identifier as Attribute on Statement.attribute = Attribute.rowid 
                left join Identifier as Value on Statement.value_identifier = Value.rowid
                where dataset.name = @name"

            let param = [ "name", SqlType.String(datasetName dataset) ]

            let results =
                conn
                |> Db.newCommand sql
                |> Db.setParams param
                |> Db.setTransaction tx
                |> Db.query statementDataReader //TODO use Db.read not Db.query

            match results with
            | Ok(r) -> List.traverseResultM (fun r -> r) r
            | Error(dbError) -> error "Error reading Statements." (Some $"DB Error - {dbError}")

        member _.MatchStatements entity attribute value =
            //TODO I'm not handling errors properly in this function
            let entity = Option.map (fun e -> lookupIdentifier e) entity
            let attribute = Option.map (fun a -> lookupIdentifier a) attribute
            let value = Option.map (fun v -> createValueParams v) value

            let entityQuery =
                match entity with
                | Some(entityId) -> "entity = @entity"
                | None -> ""

            let attributeQuery =
                match attribute with
                | Some(attributeId) -> "attribute = @attribute"
                | None -> ""

            let valueQuery =
                match value with
                | Some(Ok(value)) -> createValueQuery value
                | None -> ""
                | _ -> todo //should never reach?

            let queryCondition: string =
                [ entityQuery; attributeQuery; valueQuery ]
                |> List.fold
                    (fun query nextElement ->
                        query
                        + if nextElement = "" then
                              nextElement
                          else
                              " and " + nextElement)
                    ""

            let sql =
                $"select Entity.identifier as entity, Attribute.identifier as attribute, Value.identifier as value_identifier,
                Statement.value_string as value_string, Statement.value_integer as value_integer from Dataset
                inner join Statement on Dataset.rowid = Statement.dataset 
                inner join Identifier as Entity on statement.entity = Entity.rowid 
                inner join Identifier as Attribute on Statement.attribute = Attribute.rowid 
                left join Identifier as Value on Statement.value_identifier = Value.rowid
                where dataset.name = @name
                {queryCondition}"
            //let param = [ "dataset", SqlType.Int64 datasetId ] //TODO rewrite query above to use Dataset id not name
            let param = [ "name", SqlType.String(datasetName dataset) ]

            let results =
                conn
                |> Db.newCommand sql
                |> Db.setParams param
                |> Db.setTransaction tx
                |> Db.query statementDataReader //TODO use Db.read not Db.query

            match results with
            | Ok(r) -> List.traverseResultM (fun r -> r) r
            | Error(dbError) -> error "Error reading Statements." (Some $"DB Error - {dbError}")
