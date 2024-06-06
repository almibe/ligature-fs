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

    interface System.IComparable with
        member this.CompareTo(other) =
            let other = other :?> Slot
            this.Name.CompareTo(other.Name)

let slot name = Slot.New name

let slotUnsafe name =
    match Slot.New name with
    | Ok(slot) -> slot
    | Error(_) -> failwith "Error"

[<RequireQualifiedAccess>]
type PatternIdentifier =
    | Sl of Slot
    | Id of Identifier

[<RequireQualifiedAccess>]
type Value =
    | Slot of Slot
    | Identifier of Identifier
    | String of string
    | Int of bigint
    | Bytes of byte array

[<CustomEquality; CustomComparison>]
type Statement =
    { Entity: PatternIdentifier
      Attribute: PatternIdentifier
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

let getRoots (patternSet: Set<Statement>) : Set<PatternIdentifier> =
    Set.map (fun (statement: Statement) -> statement.Entity) patternSet

let getLeaves (patternSet: Set<Statement>) : Set<PatternIdentifier> =
    patternSet
    |> Set.map (fun (statement: Statement) ->
        match statement.Value with
        | Value.Identifier identifier -> Some(PatternIdentifier.Id identifier)
        | Value.Slot slot -> Some(PatternIdentifier.Sl slot)
        | _ -> None)
    |> Set.filter (fun x -> x.IsSome)
    |> Set.map (fun x -> x.Value)


let readIdentifier (Identifier identifier) = identifier

let readPatternIdentifier (identifier: PatternIdentifier) : string =
    match identifier with
    | PatternIdentifier.Id(Identifier identifier) -> identifier
//  | PatternIdentifier.Sl(Slot(name)) -> name
    | _ -> failwith "TODO"

type Network(statements: Set<Statement>) =
    member _.Count() : int64 = 
        Set.count statements
    member _.AllStatements() : Set<Statement> = 
        statements
    member _.Matches(other: Network) : bool =
        failwith "TODO"
    member _.Extract(other: Network): Map<Slot, Value> list = 
        failwith "TODO"

let statement entity attribute value =
    { Entity = entity
      Attribute = attribute
      Value = value }
