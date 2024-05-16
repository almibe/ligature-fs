// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Main

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

[<RequireQualifiedAccess>]
type Value =
    | Identifier of Identifier
    | String of string
    | Int of bigint
    | Bytes of byte array

[<CustomEquality; CustomComparison>]
type Statement =
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
        HashCode.Combine(this.Entity, this.Attribute, this.Value)

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

type INetwork =
    abstract member all: unit -> Statement seq
    abstract member find: Identifier option -> Identifier option -> Value option -> Statement seq

// and INetwork =
//     abstract member Extract: IPattern -> Map<Slot, Value> list
//     abstract member Contains: IPattern -> Result<bool, LigatureError>
//     abstract member Count: IPattern -> Result<int64, LigatureError>
//     abstract member AllStatements: unit -> Result<Statement list, LigatureError>

let statement entity attribute value =
    { Entity = entity
      Attribute = attribute
      Value = value }
