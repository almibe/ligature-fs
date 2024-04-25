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

let invalidIdentifier (id: string) = error $"Invalid Identifier, {id}" None

let identifier =
    let identifierPattern =
        Regex(@"^[a-zA-Z0-9-._~:/?#\[\]@!$&'()*+,;%=]+$", RegexOptions.Compiled)

    fun (id: string) ->
        if identifierPattern.IsMatch(id) then
            Ok(Identifier id)
        else
            invalidIdentifier id

let readIdentifier (Identifier identifier) = identifier

type Slot = private Slot of string option

let invalidSlot (id: string) = error $"Invalid Slot, {id}" None

let slot =
    let slotPattern = Regex(@"^[a-zA-Z0-9_]+$", RegexOptions.Compiled)

    fun (id: string) ->
        if slotPattern.IsMatch(id) then
            Ok(Slot(Some id))
        else
            invalidSlot id

let readSlot (Slot slot) =
    match slot with
    | Some value -> value
    | _ -> ""

let emptySlot = Slot None

[<RequireQualifiedAccess>]
type Value =
    | Identifier of Identifier
    | String of string
    | Int of bigint
    | Bytes of byte array

[<RequireQualifiedAccess>]
type PatternIdentifier =
    | Slot of Slot
    | Identifier of Identifier

[<RequireQualifiedAccess>]
type PatternValue =
    | Slot of Slot
    | Identifier of Identifier
    | Int of bigint
    | String of string
    | Bytes of byte array

type PatternStatement =
    { Entity: PatternIdentifier
      Attribute: PatternIdentifier
      Value: PatternValue }

type Pattern = Set<PatternStatement>

type Query = Map<Pattern, Pattern>

type Statement =
    { Entity: Identifier
      Attribute: Identifier
      Value: Value }

type IDataset =
    abstract member RunQuery: Query -> Result<IDataset, LigatureError>
    abstract member Contains: Pattern -> Result<bool, LigatureError>
    abstract member Count: Pattern -> Result<int64, LigatureError>

    abstract member AllStatements: unit -> Result<Statement list, LigatureError>

type ILigature =
    abstract member AllDatasets: unit -> Result<DatasetName list, LigatureError>
    abstract member DatasetExists: DatasetName -> Result<bool, LigatureError>
    abstract member CreateDataset: DatasetName -> Result<Unit, LigatureError>
    abstract member RemoveDataset: DatasetName -> Result<Unit, LigatureError>
    abstract member AllStatements: DatasetName -> Result<Statement list, LigatureError>
    abstract member Contains: DatasetName -> Pattern -> Result<bool, LigatureError>
    abstract member Count: DatasetName -> Pattern -> Result<int64, LigatureError>
    abstract member RunQuery: DatasetName -> Query -> Result<IDataset, LigatureError>
    abstract member Call: DatasetName -> Identifier -> IDataset -> Result<IDataset, LigatureError>
    abstract member AddStatements: DatasetName -> Statement list -> Result<unit, LigatureError>
    abstract member RemoveStatements: DatasetName -> Statement list -> Result<unit, LigatureError>
    abstract member Close: unit -> Result<Unit, LigatureError>

let statement entity attribute value =
    { Entity = entity
      Attribute = attribute
      Value = value }

let tryPatternToStatement (pattern: Pattern) : Statement list option =
    Set.map
        (fun (statementPattern: PatternStatement) ->
            let entity =
                match statementPattern.Entity with
                | PatternIdentifier.Identifier identifier -> identifier
                | _ -> failwith ""

            let attribute =
                match statementPattern.Attribute with
                | PatternIdentifier.Identifier identifier -> identifier
                | _ -> failwith ""

            let value =
                match statementPattern.Value with
                | PatternValue.Identifier identifier -> Value.Identifier identifier
                | _ -> failwith ""

            { Entity = entity
              Attribute = attribute
              Value = value })
        pattern
    |> List.ofSeq
    |> Some
