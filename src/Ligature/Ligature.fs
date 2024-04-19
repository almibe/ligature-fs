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

type Range =
    | String of string * string
    | Integer of bigint * bigint
    | Bytes of byte array * byte array

type Function = IDataset -> IDataset

and [<RequireQualifiedAccess>] Value =
    | Identifier of Identifier
    | String of string
    | Integer of bigint
    | Bytes of byte array
    | Function of Function

and ValueQuery =
    | Value of Value
    | Range of Range

and Statement =
    { Entity: Identifier
      Attribute: Identifier
      Value: Value }

and IDataset =
    abstract member MatchStatements:
        Identifier option -> Identifier option -> Value option -> Result<IDataset, LigatureError>

    abstract member AllStatements: unit -> Result<Statement list, LigatureError>

type Query = IDataset -> Result<IDataset, LigatureError>

type ILigature =
    abstract member AllDatasets: unit -> Result<DatasetName list, LigatureError>
    abstract member DatasetExists: DatasetName -> Result<bool, LigatureError>
    abstract member CreateDataset: DatasetName -> Result<Unit, LigatureError>
    abstract member RemoveDataset: DatasetName -> Result<Unit, LigatureError>
    abstract member AllStatements: DatasetName -> Result<Statement list, LigatureError>
    abstract member Query: DatasetName -> Query -> Result<IDataset, LigatureError>
    abstract member AddStatements: DatasetName -> Statement list -> Result<unit, LigatureError>
    abstract member RemoveStatements: DatasetName -> Statement list -> Result<unit, LigatureError>
    abstract member Close: unit -> Result<Unit, LigatureError>

let statement entity attribute value =
    { Entity = entity
      Attribute = attribute
      Value = value }
