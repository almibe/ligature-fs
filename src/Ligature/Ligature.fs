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
    | Network
    | Quote
    | Expression
    | Any

and Combinator =
    { Name: Symbol
      Doc: string
      Signature: LigatureType list * LigatureType option
      Eval: Combinators -> LigatureStore -> Arguments -> Result<WanderValue option, LigatureError> }

and [<RequireQualifiedAccess; StructuralEquality; StructuralComparison>] WanderValue =
    | Symbol of Symbol
    | Quote of Quote
    | Expression of Expression
    | Network of Network

and Concept = (Symbol * Symbol)

and NotConcept = (Symbol * Symbol)

and Role = (Symbol * Symbol * Symbol)

and Entry =
    | Concept of Concept
    | NotConcept of NotConcept
    | Role of Role

and Network = Set<Entry>

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

// let readBinding (name: Pattern) (network: Network) : Option<Pattern> =
//     let res =
//         Set.filter
//             (fun (e, a, _) ->
//                 match (name, e, a) with
//                 | (Pattern.Symbol(name), Pattern.Symbol(entity), Pattern.Symbol(Symbol("="))) -> entity = name
//                 | (Pattern.Slot(slot), Pattern.Slot(entity), Pattern.Symbol(Symbol("="))) -> entity = slot
//                 | _ -> false)
//             network

//     match List.ofSeq (res) with
//     | [] -> None
//     | [ (_, _, value) ] -> Some(value) //evalQuote hostFunctions runtimeNetwork quote
//     | _ -> None

// let getRoots (patternSet: Set<Statement>) : Set<Symbol> =
//     Set.map (fun ((entity, _, _): Statement) -> entity) patternSet


let printSymbol (Symbol(symbol)) : string = symbol
