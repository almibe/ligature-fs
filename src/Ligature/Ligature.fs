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
    member _.Count() : int64 = Set.count statements
    member _.AllStatements() : Set<Statement> = statements

    member this.Apply(data: Map<Slot, Value>) : Network =
        let res: Set<Statement> =
            Set.map
                (fun (statement: Statement) ->
                    match statement with
                    // | { Entity = PatternIdentifier.Id(_)
                    //     Attribute = PatternIdentifier.Id(_)
                    //     Value = Value(_) } -> failwith "TODO"
                    | _ ->
                        let entity =
                            match statement.Entity with
                            | PatternIdentifier.Id(identifier) -> identifier
                            | PatternIdentifier.Sl(slot) ->
                                if slot.Named then
                                    match data.TryFind slot with
                                    | Some value ->
                                        match value with
                                        | Value.Identifier identifier -> identifier
                                        | _ -> failwith "Error"
                                    | None -> failwith "Error"
                                else
                                    failwith "Error"

                        let attribute =
                            match statement.Attribute with
                            | PatternIdentifier.Id(identifier) -> identifier
                            | PatternIdentifier.Sl(slot) ->
                                if slot.Named then
                                    match data.TryFind slot with
                                    | Some value ->
                                        match value with
                                        | Value.Identifier identifier -> identifier
                                        | _ -> failwith "Error"
                                    | None -> failwith "Error"
                                else
                                    failwith "Error"

                        let value =
                            match statement.Value with
                            | Value.Slot(slot) ->
                                if slot.Named then
                                    match data.TryFind slot with
                                    | Some value -> value
                                    | None -> failwith "Error"
                                else
                                    failwith "Error"
                            | v -> v

                        { Entity = (PatternIdentifier.Id entity)
                          Attribute = (PatternIdentifier.Id attribute)
                          Value = value })
                statements

        Network(res)

    member this.Extract(pattern: Network) : Map<Slot, Value> list =
        if statements.IsEmpty || pattern.AllStatements().IsEmpty then
            List.Empty
        else
            let mutable res: Map<Slot, Value> list = List.empty //TODO make a list
            let mutable currentNames: Map<Slot, Value> = Map.empty

            statements
            |> Set.iter (fun statement ->
                currentNames <- Map.empty //reset state

                pattern.AllStatements()
                |> Set.iter (fun (pattern: Statement) ->
                    let mutable matched = true

                    match pattern.Entity with
                    | PatternIdentifier.Id identifier -> matched <- statement.Entity = PatternIdentifier.Id identifier
                    | PatternIdentifier.Sl slot ->
                        if slot.Named then
                            if currentNames.ContainsKey slot then
                                failwith "TODO"
                            else
                                match statement.Entity with
                                | PatternIdentifier.Id identifier ->
                                    currentNames <- currentNames.Add(slot, Value.Identifier identifier)
                                | PatternIdentifier.Sl slot -> failwith "Error"
                        else
                            ()

                    match pattern.Attribute with
                    | PatternIdentifier.Id identifier ->
                        matched <- statement.Attribute = PatternIdentifier.Id identifier
                    | PatternIdentifier.Sl slot ->
                        if slot.Named then
                            if currentNames.ContainsKey slot then
                                failwith "TODO"
                            else
                                match statement.Attribute with
                                | PatternIdentifier.Id identifier ->
                                    currentNames <- currentNames.Add(slot, Value.Identifier identifier)
                                | _ -> failwith "Error"
                        else
                            ()

                    match pattern.Value with
                    | Value.Slot slot ->
                        if slot.Named then
                            if currentNames.ContainsKey slot then
                                failwith "TODO"
                            else
                                currentNames <- currentNames.Add(slot, statement.Value)
                        else
                            ()
                    | value -> matched <- statement.Value = value

                    if matched then
                        res <- List.append res [ currentNames ]))

            List.ofSeq res

let statement entity attribute value =
    { Entity = entity
      Attribute = attribute
      Value = value }
