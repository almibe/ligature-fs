// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature

open System.Text.RegularExpressions

type LigatureError = { 
    userMessage: string
    debugMessage: string option
}

let error userMessage debugMessage = Error({
    userMessage = userMessage
    debugMessage = debugMessage
})

type Dataset = private Dataset of string

let datasetPattern = Regex(@"^([a-zA-Z_][a-zA-Z0-9_]*)(/[a-zA-Z_][a-zA-Z0-9_]*)*$", RegexOptions.Compiled)

let invalidDataset (datasetName: string) =
    error $"Invalid Dataset name, {datasetName}" None

let dataset name = 
    if datasetPattern.IsMatch(name) then
        Ok(Dataset name)
    else
        invalidDataset name

let readDataset dataset =
    match dataset with
    | Dataset(name) -> name

type Identifier = private Identifier of string

let identifierPattern = Regex(@"^[a-zA-Z0-9-._~:/?#\[\]@!$&'()*+,;%=]+$", RegexOptions.Compiled)

let invalidIdentifier (id: string) = 
    error $"Invalid Identifier, {id}" None

let identifier id =
    if identifierPattern.IsMatch(id) then
        Ok(Identifier id)
    else
        invalidIdentifier id

let readIdentifier (identifier: Identifier) =
    match identifier with
    | Identifier(id) -> id

type Value =
    | Identifier of Identifier
    | String of string
    | Integer of int64
    //TODO add Bytes

type Statement = {
    Entity: Identifier
    Attribute: Identifier
    Value: Value
}

type QueryTx =
    abstract member AllStatements: unit -> Result<Statement list, LigatureError>
    abstract member MatchStatements: Identifier option -> Identifier option -> Value option -> Result<Statement list, LigatureError>
    //TODO add MatchStatementsRange
    
type WriteTx =
    abstract member NewIdentifier: unit -> Result<Identifier, LigatureError>
    abstract member AddStatement: Statement -> Result<unit, LigatureError>
    abstract member RemoveStatement: Statement -> Result<unit, LigatureError>

type Query<'r> = QueryTx -> Result<'r, LigatureError>

type Write = WriteTx -> Result<unit, LigatureError>

type Ligature =
    abstract member AllDatasets: unit -> Result<Dataset list, LigatureError>
    abstract member DatasetExists: Dataset -> Result<bool, LigatureError>
    abstract member CreateDataset: Dataset -> Result<Unit, LigatureError>
    abstract member RemoveDataset: Dataset -> Result<Unit, LigatureError>
    abstract member Query: Dataset -> Query<'r> -> Result<'r, LigatureError>
    abstract member Write: Dataset -> Write -> Result<unit, LigatureError>
    abstract member Close: unit -> Result<Unit, LigatureError>
