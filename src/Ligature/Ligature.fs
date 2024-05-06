// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature

open System.Text.RegularExpressions
open System

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

type Slot private (name: string option) =
    member _.Named = name.IsSome

    member _.Name =
        match name with
        | Some name -> name
        | None -> ""

    static member New(name: string option) =
        let slotPattern = Regex(@"^[a-zA-Z0-9_]+$", RegexOptions.Compiled)
        let invalidSlot (id: string) = error $"Invalid Slot, {id}" None

        match name with
        | Some name ->
            if slotPattern.IsMatch(name) then
                Ok(Slot(Some name))
            else
                invalidSlot name
        | None -> Ok(Slot(None))

    static member Empty = Slot(None)

    override this.Equals(other) =
        let other = other :?> Slot
        this.Name = other.Name

    interface IComparable with
        member this.CompareTo(other) =
            let other = other :?> Slot
            this.Name.CompareTo(other.Name)

let slot name = Slot.New name

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
    | Value of Value

type PatternStatement =
    { Entity: PatternIdentifier
      Attribute: PatternIdentifier
      Value: PatternValue }

type IPattern =
    abstract member PatternStatements: Set<PatternStatement>
    abstract member Apply: Map<Slot, Value> -> IDataset option
    abstract member Dataset: IDataset option
    abstract member SingleRoot: bool

and [<CustomEquality; CustomComparison>] Statement =
    { Entity: Identifier
      Attribute: Identifier
      Value: Value }

    override this.Equals(other) =
        match other with
        | :? Statement as other ->
            this.Entity = other.Entity
            && this.Attribute = other.Attribute
            && this.Value = other.Value
        | _ -> false

    override this.GetHashCode() =
        #if !FABLE_COMPILER
            HashCode.Combine(this.Entity, this.Attribute, this.Value)
        #else
            let mutable hash = 17
            hash <- hash * 31 + this.Entity.GetHashCode()
            hash <- hash * 31 + this.Attribute.GetHashCode()
            hash <- hash * 31 + this.Value.GetHashCode()
            hash
        #endif

    interface IComparable with
        member this.CompareTo(other) =
            match other with
            | :? Statement as other ->
                let e = other.Entity.ToString().CompareTo(this.Entity.ToString())
                let a = other.Attribute.ToString().CompareTo(this.Attribute.ToString())
                let v = other.Value.ToString().CompareTo(this.Value.ToString())

                if e <> 0 then e
                else if a <> 0 then a
                else v
            | _ -> failwith "Error"

and IDataset =
    abstract member Extract: IPattern -> Map<Slot, Value> list
    abstract member Contains: IPattern -> Result<bool, LigatureError>
    abstract member Count: IPattern -> Result<int64, LigatureError>
    abstract member AllStatements: unit -> Result<Statement list, LigatureError>

type ILigature =
    abstract member AllDatasets: unit -> Result<DatasetName list, LigatureError>
    abstract member DatasetExists: DatasetName -> Result<bool, LigatureError>
    abstract member CreateDataset: DatasetName -> Result<Unit, LigatureError>
    abstract member RemoveDataset: DatasetName -> Result<Unit, LigatureError>
    abstract member AllStatements: DatasetName -> Result<Statement list, LigatureError>
    abstract member Contains: DatasetName -> IPattern -> Result<bool, LigatureError>
    abstract member Count: DatasetName -> IPattern -> Result<int64, LigatureError>
    abstract member Extract: DatasetName -> IPattern -> Result<Map<Slot, Value> array, LigatureError>
    abstract member Call: DatasetName -> Identifier -> IDataset -> Result<IDataset, LigatureError>
    abstract member AddStatements: DatasetName -> Statement list -> Result<unit, LigatureError>
    abstract member RemoveStatements: DatasetName -> Statement list -> Result<unit, LigatureError>
    abstract member Close: unit -> Result<Unit, LigatureError>

let statement entity attribute value =
    { Entity = entity
      Attribute = attribute
      Value = value }
