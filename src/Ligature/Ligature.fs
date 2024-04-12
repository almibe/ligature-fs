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

type DatasetName = DatasetName of string

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

[<RequireQualifiedAccess>]
type Value =
    | Identifier of Identifier
    | String of string
    | Integer of int64
    | Bytes of byte array

type Range =
    | String of string * string
    | Integer of int64 * int64
    | Bytes of byte array * byte array

type ValueQuery =
    | Value of Value
    | Range of Range

type Statement =
    { Entity: Identifier
      Attribute: Identifier
      Value: Value }

let statement entity attribute value =
    { Entity = entity
      Attribute = attribute
      Value = value }

type IDataset =
    abstract member MatchStatements:
        Identifier option -> Identifier option -> Value option -> Result<IDataset, LigatureError>

    abstract member AllStatements: unit -> Result<Statement list, LigatureError>

type Query = IDataset -> Result<IDataset, LigatureError>

type ILigature =
    abstract member AllDatasets: unit -> Result<DatasetName list, LigatureError>
    abstract member DatasetExists: DatasetName -> Result<bool, LigatureError>
    abstract member CreateDataset: DatasetName -> Result<Unit, LigatureError>
    abstract member RemoveDataset: DatasetName -> Result<Unit, LigatureError>
    abstract member Query: DatasetName -> Query -> Result<'r, LigatureError>
    abstract member AddStatements: DatasetName -> Statement list -> Result<unit, LigatureError>
    abstract member RemoveStatements: DatasetName -> Statement list -> Result<unit, LigatureError>
    abstract member Close: unit -> Result<Unit, LigatureError>
