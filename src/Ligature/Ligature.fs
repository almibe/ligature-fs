// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Main

type LigatureError =
    { UserMessage: string
      DebugMessage: string option }

let error userMessage debugMessage =
    Error(
        { UserMessage = userMessage
          DebugMessage = debugMessage }
    )

type Slot = Slot of string option

type Symbol = Symbol of string

and [<RequireQualifiedAccessAttribute>] Element =
    | Expression of Expression
    | Network of Symbol * Network

and Quote = WanderValue list

and Expression = WanderValue list

and Combinators = Map<Symbol, Combinator>

and Arguments = WanderValue list

and [<RequireQualifiedAccessAttribute>] LigatureType =
    | String
    | Int
    | Bytes
    | Symbol
    | Slot
    | Network
    | Quote
    | Expression
    | Any

and Combinator =
    { Name: Symbol
      Doc: string
      Signature: LigatureType list * LigatureType option
      Eval: Combinators -> LigatureStore -> Arguments -> Result<WanderValue option, LigatureError> }

and [<RequireQualifiedAccess; StructuralEquality; StructuralComparison>] Identifier =
    | Slot of Slot
    | Symbol of Symbol

and [<RequireQualifiedAccess; StructuralEquality; StructuralComparison>] WanderValue =
    | Slot of Slot
    | Symbol of Symbol
    | Quote of Quote
    | Expression of Expression
    | Network of Network

and Statement = (Identifier * Identifier * Identifier)

and Network = Set<Statement>

and LigatureStore =
    abstract Networks: unit -> Symbol seq
    abstract AddNetwork: Symbol -> unit
    abstract RemoveNetwork: Symbol -> unit
    abstract ClearNetwork: Symbol -> unit
    abstract Add: Symbol -> Network -> Result<unit, LigatureError>
    abstract Set: Symbol -> Network -> Result<unit, LigatureError>
    abstract Remove: Symbol -> Network -> Result<unit, LigatureError>
    abstract Query: Symbol -> Network -> Network
    abstract Read: Symbol -> Network

let defaultNetwork = Symbol("")

let currentNetwork name networks : Network = failwith "TODO"
// match Map.tryFind name networks with
// | Some res -> res
// | None -> Set.empty

let readBinding (name: Identifier) (network: Network) : Option<Identifier> =
    let res =
        Set.filter
            (fun (e, a, _) ->
                match (name, e, a) with
                | (Identifier.Symbol(name), Identifier.Symbol(entity), Identifier.Symbol(Symbol("="))) -> entity = name
                | (Identifier.Slot(slot), Identifier.Slot(entity), Identifier.Symbol(Symbol("="))) -> entity = slot
                | _ -> false)
            network

    match List.ofSeq (res) with
    | [] -> None
    | [ (_, _, value) ] -> Some(value) //evalQuote hostFunctions runtimeNetwork quote
    | _ -> None

let getRoots (patternSet: Set<Statement>) : Set<Identifier> =
    Set.map (fun ((entity, _, _): Statement) -> entity) patternSet

let getLeaves (patternSet: Set<Statement>) : Set<Identifier> =
    patternSet
    |> Set.map (fun ((_, _, value): Statement) ->
        match value with
        | Identifier.Symbol identifier -> Some(Identifier.Symbol identifier)
        | Identifier.Slot slot -> Some(Identifier.Slot slot)
        | _ -> None)
    |> Set.filter (fun x -> x.IsSome)
    |> Set.map (fun x -> x.Value)

let printIdentifier (pattern: Identifier) : string =
    match pattern with
    | Identifier.Symbol(Symbol path) -> path
    | Identifier.Slot(Slot(Some(name))) -> $"${name}"
    | Identifier.Slot(Slot(None)) -> "$"
