// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Sqlite.WriteTx

open Ligature
open System.Data
open Donald

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

type LigatureSqliteWriteTx(dataset: Dataset, conn, tx) =
    let checkIdentifier (identifier: Identifier): Result<int64, LigatureError> =
        let identifier = readIdentifier identifier
        let sql = ""
        let params = []
        
        //TODO query db and see if identifier exists
        //TODO if it does return id
        //TODO if it doesn't insert it and return new row id
        todo
    let checkValue value =
        todo
    interface WriteTx with
        member _.NewIdentifier(): Result<Identifier,LigatureError> = 
            failwith "Not Implemented"
        member _.AddStatement(statement: Statement): Result<unit,LigatureError> = 
            let entityId = checkIdentifier statement.Entity
            let attributeId = checkIdentifier statement.Attribute
            let valueResult = checkValue statement.Value
            
            //check if statement already exists
            //add statement
            failwith "Not Implemented"
        member _.RemoveStatement(statement: Statement): Result<unit,LigatureError> = 
            failwith "Not Implemented"
