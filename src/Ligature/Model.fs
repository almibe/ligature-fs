// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Model

type LigatureError =
    { UserMessage: string
      DebugMessage: string option }

let error userMessage debugMessage =
    Error
        { UserMessage = userMessage
          DebugMessage = debugMessage }

type Term = Term of string

type Literal = Literal of string

[<RequireQualifiedAccess>]
type Value = //TODO delete
    | Term of Term
    | Literal of Literal

type Slot = Slot of string

type ResultSet = Set<ValueSet>

and ValueSet = Map<Slot, Value>

and [<RequireQualifiedAccess>] TermPattern =
    | Term of Term
    | Slot of Slot

and [<RequireQualifiedAccess>] ValuePattern =
    | Term of Term
    | Literal of Literal
    | Slot of Slot

and [<RequireQualifiedAccess>] Assertion = 
    | Triple of Term * Term * Value //TODO delete
    | IsA of Term * Term
    | Relation of Term * Term * Term
    | Attrbiute of Term * Term * Literal

and Network = Set<Assertion>

and TriplePattern = TermPattern * TermPattern * ValuePattern

and Pattern = Set<TriplePattern>

// type INetwork =
//     abstract Triples: unit -> Async<Network>

type ILigatureStore =
    abstract Stores: unit -> string seq
    abstract AddStore: string -> unit
    abstract RemoveStore: string -> unit
    abstract AssertStore: string -> Network -> unit
    abstract UnassertStore: string -> Network -> unit
    abstract ReadAsserts: string -> Result<Network, LigatureError>

[<RequireQualifiedAccess>]
type ConceptExpr =
    | AtomicConcept of Term
    | Conjunction of ConceptExpr list
    | Top
    | Bottom
    | Exists of Term * ConceptExpr
    | All of Term * ConceptExpr

[<RequireQualifiedAccess>]
type Definition =
    | Implies of Term * ConceptExpr
    | Define of Term * ConceptExpr

type Definitions = Set<Definition>

let printDefinition definition = "def"

let printDefinitions definition = "defs"
