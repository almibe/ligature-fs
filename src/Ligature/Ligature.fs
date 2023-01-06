// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature

open System.Text.RegularExpressions

type LigatureError = { 
    userMessage: System.String
    debugMessage: System.String option
}

let error userMessage debugMessage = Error({
    userMessage = userMessage
    debugMessage = debugMessage
})

type Dataset = private Dataset of System.String

let datasetPattern = Regex(@"^([a-zA-Z_][a-zA-Z0-9_]*)(/[a-zA-Z_][a-zA-Z0-9_]*)*$", RegexOptions.Compiled)

let invalidDataset (datasetName: System.String) =
    error $"Invalid Dataset name, {datasetName}" None

let dataset name = 
    if datasetPattern.IsMatch(name) then
        Ok(Dataset name)
    else
        invalidDataset name

let readDataset dataset =
    match dataset with
    | Dataset(name) -> name

type Identifier = private Identifier of System.String

let identifierPattern = Regex(@"^[a-zA-Z0-9-._~:/?#\[\]@!$&'()*+,;%=]+$", RegexOptions.Compiled)

let invalidIdentifier (id: System.String) = 
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
    | String of System.String
    | Integer of int64
    //TODO add Bytes

type Statement = {
    Entity: Identifier
    Attribute: Identifier
    Value: Value
}

type QueryTx =
    abstract member AllStatements: unit -> Result<Statement array, LigatureError>
    abstract member MatchStatements: Identifier option -> Identifier option -> Value option -> Statement array
    //TODO add MatchStatementsRange
    
type WriteTx =
    abstract member NewIdentifier: unit -> Result<Identifier, LigatureError>
    abstract member AddStatement: Statement -> Result<unit, LigatureError>
    abstract member RemoveStatement: Statement -> Result<unit, LigatureError>

type Query<'r> = QueryTx -> 'r

type Write = WriteTx -> unit

type Ligature =
    abstract member AllDatasets: unit -> Result<Dataset array, LigatureError>
    abstract member DatasetExists: Dataset -> bool
    abstract member CreateDataset: Dataset -> Result<Unit, LigatureError>
    abstract member RemoveDataset: Dataset -> Result<Unit, LigatureError>
    abstract member Query: Dataset -> Query<'r> -> 'r
    abstract member Write: Dataset -> Write -> unit
    abstract member Close: unit -> Result<Unit, LigatureError>
