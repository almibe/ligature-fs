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

and Element = Symbol

and ConceptName = Symbol

and RoleName = Symbol

and Extension =
    { element: Symbol
      concept: ConceptName }

and NonExtension =
    { element: Symbol
      concept: ConceptName }

and Role =
    { first: Symbol
      second: Symbol
      role: RoleName }

and [<RequireQualifiedAccess>] Entry =
    | Extension of Extension
    | NonExtension of NonExtension
    | Role of Role

and Network = Set<Entry>

and NetworkName = string

and LigatureStore =
    abstract Networks: unit -> Set<NetworkName>
    abstract Read: NetworkName -> Network
    abstract IsConsistent: NetworkName -> bool
    abstract IsComplete: NetworkName -> bool
    abstract AllConcepts: NetworkName -> Set<ConceptName>
    abstract AllRoles: NetworkName -> Set<RoleName>
    abstract AllExtentions: NetworkName -> ConceptName -> Set<Symbol>
    abstract AllRoleInstances: NetworkName -> RoleName -> Set<Role>
    abstract Find: NetworkName -> Pattern -> Set<Map<Variable, Symbol>>

    abstract AddNetwork: NetworkName -> unit
    abstract RemoveNetwork: NetworkName -> unit
    abstract ClearNetwork: NetworkName -> unit
    abstract Add: NetworkName -> Network -> Result<unit, LigatureError>
    abstract Set: NetworkName -> Network -> Result<unit, LigatureError>
    abstract Remove: NetworkName -> Network -> Result<unit, LigatureError>

and [<RequireQualifiedAccess>] QueryTerm =
    | ConceptTerm of Slot * ConceptNameSlot
    | RoleTerm of Slot * Slot * RoleNameSlot

and Pattern = Set<QueryTerm>

and Variable = string

and [<RequireQualifiedAccess>] ConceptNameSlot =
    | ConceptName of ConceptName
    | Variable of Variable

and [<RequireQualifiedAccess>] RoleNameSlot =
    | RoleName of RoleName
    | Variable of string

and [<RequireQualifiedAccess>] Slot =
    | Element of Symbol
    | Variable of string

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
