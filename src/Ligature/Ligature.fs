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

// type Identifier = private Identifier of string

// let invalidIdentifier (id: string) = error $"Invalid Identifier, {id}" None

// let identifier =
//     let identifierPattern =
//         Regex(@"^[a-zA-Z0-9-._~:/?#\[\]@!$&'()*+,;%=]+$", RegexOptions.Compiled)

//     fun (id: string) ->
//         if identifierPattern.IsMatch(id) then
//             Ok(Identifier id)
//         else
//             invalidIdentifier id

type Slot = Slot of string option

type Word = Word of string

// type Slot = private Slot of string option
//     member _.Named = name.IsSome

//     member _.Name =
//         match name with
//         | Some name -> name
//         | None -> ""

//     static member New(name: string option) =
//         let slotPattern = Regex(@"^[a-zA-Z0-9_]+$", RegexOptions.Compiled)
//         let invalidSlot (id: string) = error $"Invalid Slot, {id}" None

//         match name with
//         | Some name ->
//             if slotPattern.IsMatch(name) then
//                 Ok(Slot(Some name))
//             else
//                 invalidSlot name
//         | None -> Ok(Slot(None))

//     static member Empty = Slot(None)

//     override this.Equals(other) =
//         let other = other :?> Slot
//         this.Name = other.Name

//     override this.GetHashCode() = name.GetHashCode()

//     interface System.IComparable with
//         member this.CompareTo(other) =
//             let other = other :?> Slot
//             this.Name.CompareTo(other.Name)

// let slot name = Slot.New name

// let slotUnsafe name =
//     match Slot.New name with
//     | Ok(slot) -> slot
//     | Error(_) -> failwith "Error"

type Arguments = LigatureValue list

and [<StructuralEquality; StructuralComparison>] Quote =
    { parameterNames: string list
      quote: LigatureValue list }

and Combinator =
    { Eval: Network -> Result<Network, LigatureError> }

and Words = Map<string, Combinator>

and [<RequireQualifiedAccess; StructuralEquality; StructuralComparison>] PatternWord =
    | Slot of Slot
    | Word of Word

and [<RequireQualifiedAccess; StructuralEquality; StructuralComparison>] LigatureValue =
    | Slot of Slot
    | Word of Word
    | String of string
    | Int of bigint
    | Bytes of byte array
    | Pipeline of LigatureValue list
    | Network of Network

and Statement = (PatternWord * PatternWord * LigatureValue)

and Network = Set<Statement>

and NetworkName = string

and Networks = Map<NetworkName, Network>

and State = NetworkName * Networks

let defaultState: State = ("", Map.ofSeq ([ ("", Set.empty) ]))

let currentNetwork ((name, networks): State) : Network =
    match Map.tryFind name networks with
    | Some res -> res
    | None -> Set.empty
// and [<StructuralEquality; StructuralComparison>] Network =
//     abstract member Write: unit -> Set<Statement>
//     abstract member Count: unit -> int64
//     abstract member Union: Network -> Network
//     abstract member Minus: Network -> Network
//     abstract member Apply: Map<string, LigatureValue> -> Network
//     abstract member Educe: Network -> Set<Map<string, LigatureValue>>
//     abstract member Query: Network -> Network -> Network
//     abstract member Infer: Network -> Network -> Network

let getRoots (patternSet: Set<Statement>) : Set<PatternWord> =
    Set.map (fun ((entity, _, _): Statement) -> entity) patternSet

let getLeaves (patternSet: Set<Statement>) : Set<PatternWord> =
    patternSet
    |> Set.map (fun ((_, _, value): Statement) ->
        match value with
        | LigatureValue.Word word -> Some(PatternWord.Word word)
        | LigatureValue.Slot slot -> Some(PatternWord.Slot slot)
        | _ -> None)
    |> Set.filter (fun x -> x.IsSome)
    |> Set.map (fun x -> x.Value)

// let readIdentifier (Identifier identifier) = identifier

let readPatternWord (pattern: PatternWord) : string =
    match pattern with
    | PatternWord.Word(Word word) -> word
    //  | PatternWord.Sl(Slot(name)) -> name
    | _ -> failwith "TODO"
