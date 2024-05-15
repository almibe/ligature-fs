// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.InMemory.Pattern

open Ligature.Main
open Dataset
open System.Text.RegularExpressions

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
    abstract member Apply: Map<Slot, Value> -> INetwork option
    abstract member Dataset: INetwork option
    abstract member SingleRoot: bool

let getRoots (patternSet: Set<PatternStatement>): Set<PatternIdentifier> = 
    Set.map (fun (statement: PatternStatement) -> statement.Entity) patternSet

let getLeaves (patternSet: Set<PatternStatement>): Set<PatternIdentifier> = 
    patternSet
    |> Set.map (fun (statement: PatternStatement) ->
        match statement.Value with
        | PatternValue.Value value ->
            match value with
            | Value.Identifier identifier -> Some (PatternIdentifier.Identifier identifier)
            | _ -> None
        | PatternValue.Slot slot -> Some(PatternIdentifier.Slot slot))
    |> Set.filter (fun x -> x.IsSome)
    |> Set.map (fun x -> x.Value)

type InMemoryPattern(patternStatements: Set<PatternStatement>) =
    override _.Equals(other) =
        match other with
        | :? IPattern as other -> patternStatements = other.PatternStatements
        | _ -> false

    override _.GetHashCode() = patternStatements.GetHashCode()

    interface IPattern with
        member _.PatternStatements = patternStatements

        member _.Apply(data: Map<Slot, Value>) =
            let res: Set<Statement> =
                Set.map
                    (fun (statement: PatternStatement) ->
                        match statement with
                        | { Entity = PatternIdentifier.Identifier(_)
                            Attribute = PatternIdentifier.Identifier(_)
                            Value = PatternValue.Value(_) } -> failwith "TODO"
                        | _ ->
                            let entity =
                                match statement.Entity with
                                | PatternIdentifier.Identifier(identifier) -> identifier
                                | PatternIdentifier.Slot(slot) ->
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
                                | PatternIdentifier.Identifier(identifier) -> identifier
                                | PatternIdentifier.Slot(slot) ->
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
                                | PatternValue.Value(v) -> v
                                | PatternValue.Slot(slot) ->
                                    if slot.Named then
                                        match data.TryFind slot with
                                        | Some value -> value
                                        | None -> failwith "Error"
                                    else
                                        failwith "Error"

                            { Entity = entity
                              Attribute = attribute
                              Value = value })
                    patternStatements

            Some(InMemoryNetwork(res))

        member _.Dataset: INetwork option =
            let res: Set<Statement> =
                Set.map
                    (fun (statement: PatternStatement) ->
                        match statement with
                        | { Entity = PatternIdentifier.Identifier(e)
                            Attribute = PatternIdentifier.Identifier(a)
                            Value = PatternValue.Value(v) } -> { Entity = e; Attribute = a; Value = v }
                        | _ ->
                            let entity =
                                match statement.Entity with
                                | PatternIdentifier.Identifier identifier -> identifier
                                | PatternIdentifier.Slot _ -> failwith "Error"

                            let attribute =
                                match statement.Attribute with
                                | PatternIdentifier.Identifier identifier -> identifier
                                | PatternIdentifier.Slot _ -> failwith "TODO"

                            let value =
                                match statement.Value with
                                | PatternValue.Value v -> v
                                | PatternValue.Slot _ -> failwith "Error"

                            { Entity = entity
                              Attribute = attribute
                              Value = value })
                    patternStatements

            Some(InMemoryNetwork(res))
        member _.SingleRoot: bool = 
            let roots = getRoots patternStatements
            let leaves = getLeaves patternStatements
            let root = Set.difference roots leaves
            Set.count root = 1

let emptyPattern: IPattern = InMemoryPattern(Set.empty)

let unsafeDatasetToPattern (dataset: INetwork) : IPattern =
    failwith "TODO"
    // match dataset.AllStatements() with
    // | Ok res ->
    //     let (l: PatternStatement list) =
    //         List.map
    //             (fun item ->
    //                 { Entity = PatternIdentifier.Identifier item.Entity
    //                   Attribute = PatternIdentifier.Identifier item.Attribute
    //                   Value = PatternValue.Value item.Value })
    //             res

    //     InMemoryPattern(Set.ofList l)
    // | _ -> failwith "Error"
