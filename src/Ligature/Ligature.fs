// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature

open System.Text.RegularExpressions

type LigatureError =
    { UserMessage: string
      DebugMessage: string option }

let error userMessage debugMessage =
    Error(
        { UserMessage = userMessage
          DebugMessage = debugMessage }
    )

type Dataset = Dataset of string

type Identifier = private Identifier of string

let invalidIdentifier (id: string) = error $"Invalid Label, {id}" None

let identifier =
    let identifierPattern =
        Regex(@"^[a-zA-Z0-9-._~:/?#\[\]@!$&'()*+,;%=]+$", RegexOptions.Compiled)

    fun (id: string) ->
        if identifierPattern.IsMatch(id) then
            Ok(Identifier id)
        else
            invalidIdentifier id

let readIdentifier (Identifier identifier) = identifier

type Value =
    | Identifier of Identifier
    | String of string
    | Integer of int64
//TODO add Bytes

type Statement =
    { Entity: Identifier
      Attribute: Identifier
      Value: Value }

let statement entity attribute value =
    {
        Entity = entity
        Attribute = attribute
        Value = value
    }

type IQueryTx =
    abstract member AllStatements: unit -> Result<Statement list, LigatureError>

    abstract member MatchStatements:
        Identifier option -> Identifier option -> Value option -> Result<Statement list, LigatureError>
//TODO add MatchStatementsRange

type IWriteTx =
    abstract member NewIdentifier: unit -> Result<Identifier, LigatureError>
    abstract member AddStatement: Statement -> Result<unit, LigatureError>
    abstract member RemoveStatement: Statement -> Result<unit, LigatureError>

type Query<'R> = IQueryTx -> Result<'R, LigatureError>

type Write = IWriteTx -> Result<unit, LigatureError>

type ILigature =
    abstract member AllDatasets: unit -> Result<Dataset list, LigatureError>
    abstract member DatasetExists: Dataset -> Result<bool, LigatureError>
    abstract member CreateDataset: Dataset -> Result<Unit, LigatureError>
    abstract member RemoveDataset: Dataset -> Result<Unit, LigatureError>
    abstract member Query: Dataset -> Query<'r> -> Result<'r, LigatureError>
    abstract member Write: Dataset -> Write -> Result<unit, LigatureError>
    abstract member Close: unit -> Result<Unit, LigatureError>
