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
    | Triple of Term * Term * Value
    | IsA of Term * ConceptExpr

and Assertions = Set<Assertion>

and AssertionPattern = 
    | TriplePattern of TermPattern * TermPattern * ValuePattern
    | IsAPattern of TermPattern * TermPattern

and Pattern = Set<AssertionPattern>

and [<RequireQualifiedAccess>] ConceptExpr =
    | AtomicConcept of Term
    | Conjunction of ConceptExpr list
    | Top
    | Bottom
    | Exists of Term * ConceptExpr
    | All of Term * ConceptExpr
    | Not of ConceptExpr

// type INetwork =
//     abstract Triples: unit -> Async<Network>

type ILigatureStore =
    abstract Stores: unit -> string seq
    abstract AddStore: string -> unit
    abstract RemoveStore: string -> unit
    abstract AssertStore: string -> Assertions -> unit
    abstract UnassertStore: string -> Assertions -> unit
    abstract ReadAsserts: string -> Result<Assertions, LigatureError>

[<RequireQualifiedAccess>]
type Definition =
    | Implies of Term * ConceptExpr
    | Define of Term * ConceptExpr

type Definitions = Set<Definition>

let printDefinition definition = "def"

let printDefinitions definition = "defs"
