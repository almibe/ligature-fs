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

// type Name = private Name of string

// let invalidName (id: string) = error $"Invalid Name, {id}" None

// let identifier =
//     let identifierPattern =
//         Regex(@"^[a-zA-Z0-9-._~:/?#\[\]@!$&'()*+,;%=]+$", RegexOptions.Compiled)

//     fun (id: string) ->
//         if identifierPattern.IsMatch(id) then
//             Ok(Name id)
//         else
//             invalidName id

type Slot = Slot of string option

type Name = Name of string

type NetworkName = NetworkName of string

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

and [<RequireQualifiedAccessAttribute>] Element =
    | Quote of Quote
    | Name of Name
    | NetworkName of NetworkName
    | Network of Network

and Quote = { values: Element list }

and Combinators = Map<Name, Combinator>

and [<CustomEquality; CustomComparison>] Combinator =
    { Name: Name
      Doc: string
      Eval: Combinators -> State -> Result<State, LigatureError> }

    member private this.Compare other = compare this.Name other.Name

    override this.Equals other =
        match other with
        | :? Combinator as other -> this.Compare other = 0
        | _ -> false

    override this.GetHashCode() = hash this.Name

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? Combinator as other -> this.Compare other
            | _ -> -1

and [<RequireQualifiedAccess; StructuralEquality; StructuralComparison>] PatternName =
    | Slot of Slot
    | Name of Name

and [<RequireQualifiedAccess; StructuralEquality; StructuralComparison>] LigatureValue =
    | Slot of Slot
    | Name of Name
    | NetworkName of NetworkName
    | String of string
    | Int of bigint
    | Bytes of byte array
    | Quote of Quote
    | Network of Network

and Statement = (PatternName * PatternName * LigatureValue)

and Network = Set<Statement>

and Networks = Map<NetworkName, Network>

and State = NetworkName * Networks

let defaultNetwork = NetworkName("")

let defaultState: State = defaultNetwork, Map.empty

let readNetwork (name: NetworkName) ((_, networks): State) : Network =
    match Map.tryFind name networks with
    | Some res -> res
    | None -> Set.empty

let currentNetwork ((name, networks): State) : Network =
    match Map.tryFind name networks with
    | Some res -> res
    | None -> Set.empty

let readBinding (name: PatternName) (network: Network) : Option<LigatureValue> =
    let res =
        Set.filter
            (fun (e, a, _) ->
                match (name, e, a) with
                | (PatternName.Name(name), PatternName.Name(entity), PatternName.Name(Name("="))) -> entity = name
                | (PatternName.Slot(slot), PatternName.Slot(entity), PatternName.Name(Name("="))) -> entity = slot
                | _ -> false)
            network

    match List.ofSeq (res) with
    | [] -> None
    | [ (_, _, value) ] -> Some(value) //evalQuote hostFunctions runtimeNetwork quote
    | _ -> None

let getRoots (patternSet: Set<Statement>) : Set<PatternName> =
    Set.map (fun ((entity, _, _): Statement) -> entity) patternSet

let getLeaves (patternSet: Set<Statement>) : Set<PatternName> =
    patternSet
    |> Set.map (fun ((_, _, value): Statement) ->
        match value with
        | LigatureValue.Name identifier -> Some(PatternName.Name identifier)
        | LigatureValue.Slot slot -> Some(PatternName.Slot slot)
        | _ -> None)
    |> Set.filter (fun x -> x.IsSome)
    |> Set.map (fun x -> x.Value)

let printPatternName (pattern: PatternName) : string =
    match pattern with
    | PatternName.Name(Name path) -> failwith "TODO"
    | PatternName.Slot(Slot(Some(name))) -> $"${name}"
    | PatternName.Slot(Slot(None)) -> "$"
