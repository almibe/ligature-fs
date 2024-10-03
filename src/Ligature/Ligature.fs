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

type Element = Symbol of string

and [<RequireQualifiedAccessAttribute>] WanderElement =
    | Expression of Expression
    | Network of NetworkName * Set<Entry>

and Quote = Element list

// and [<RequireQualifiedAccess>] Element =
//     | Symbol of Symbol
//     | Quote of Quote

and Expression = WanderValue list

and Combinators = Map<Element, Combinator>

and Arguments = WanderValue list

and [<RequireQualifiedAccessAttribute>] LigatureType =
    | Symbol
    | Network
    | Quote
    | Expression
    | Any

and Combinator =
    { Name: Element
      Doc: string
      Signature: LigatureType list * LigatureType option
      Eval: Combinators -> LigatureStore -> Arguments -> Result<WanderValue option, LigatureError> }

and [<RequireQualifiedAccess>] WanderValue =
    | Symbol of Element
    | Quote of Quote
    | Expression of Expression
    | Network of Set<Entry>

and ConceptName = Element

and RoleName = Element

and Extension =
    { element: Element
      concept: ConceptName }

and NonExtension =
    { element: Element
      concept: ConceptName }

and Role =
    { first: Element
      second: Element
      role: RoleName }

and Entry =
    | Extension of Extension
    | NonExtension of NonExtension
    | Role of Role

and NetworkName = string

and LigatureStore =
    abstract Networks: unit -> Set<NetworkName>
    abstract AddNetwork: NetworkName -> unit
    abstract RemoveNetwork: NetworkName -> unit
    abstract ClearNetwork: NetworkName -> unit
    abstract Read: NetworkName -> Set<Entry>
    abstract Add: NetworkName -> Set<Entry> -> Result<unit, LigatureError>
    abstract Set: NetworkName -> Set<Entry> -> Result<unit, LigatureError>
    abstract Remove: NetworkName -> Set<Entry> -> Result<unit, LigatureError>
    abstract IsConsistent: NetworkName -> bool
    abstract IsComplete: NetworkName -> bool
    abstract AllConcepts: NetworkName -> Set<ConceptName>
    abstract AllRoles: NetworkName -> Set<RoleName>
    abstract AllExtentions: NetworkName -> ConceptName -> Set<Element>
    abstract AllRoleInstances: NetworkName -> RoleName -> Set<Role>
    abstract Find: NetworkName -> Set<Term> -> Set<Map<Variable, Element>>

and Term =
    | ConceptTerm of Slot * ConceptNameSlot
    | RoleTerm of Slot * Slot * RoleNameSlot

and Variable = string

and ConceptNameSlot =
    | ConceptName of ConceptName
    | Variable of Variable

and RoleNameSlot =
    | RoleName of RoleName
    | Variable of string

and Slot =
    | Element of Element
    | Variable of string

let defaultNetwork = NetworkName("")

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
