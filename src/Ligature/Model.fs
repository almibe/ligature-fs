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

type Literal =
    { content: string
      datatype: Term option
      langTag: string option }

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
    | Instance of Term * ConceptExpr
    | Same of Term * Term
    | Different of Term * Term

and ABox = Set<Assertion>

and [<RequireQualifiedAccess>] ConceptExpr =
    | AtomicConcept of Term
    | And of ConceptExpr list
    | Or of ConceptExpr list
    | Top
    | Bottom
    | Exists of Term * ConceptExpr
    | All of Term * ConceptExpr
    | Not of ConceptExpr
    | Implies of ConceptExpr * ConceptExpr
    | Equivalent of ConceptExpr * ConceptExpr
    // | Exactly of Term * ConceptExpr * int64
    // | AtLeast of Term * ConceptExpr * int64
    // | AtMost of Term * ConceptExpr * int64
    | Func of Term * ConceptExpr

// type INetwork =
//     abstract Triples: unit -> Async<Network>

type ILigatureStore =
    abstract Stores: unit -> string seq
    abstract AddStore: string -> unit
    abstract RemoveStore: string -> unit
    abstract AssertStore: string -> ABox -> unit
    abstract UnassertStore: string -> ABox -> unit
    abstract ReadAsserts: string -> Result<ABox, LigatureError>

type TBox = List<ConceptExpr>

type KnowledgeBase = TBox * ABox

let rec printConcept (concept: ConceptExpr) : string =
    match concept with
    | ConceptExpr.AtomicConcept(Term a) -> a
    | ConceptExpr.And conj ->
        List.fold
            (fun state value ->
                if state = "" then
                    printConcept value
                else
                    state + $" {printConcept value}")
            "(and"
            conj
        + ")"
    | ConceptExpr.Or disj ->
        List.fold
            (fun state value ->
                if state = "" then
                    printConcept value
                else
                    state + $" {printConcept value}")
            "(or"
            disj
        + ")"
    | ConceptExpr.Top -> "(top)"
    | ConceptExpr.Bottom -> "(bottom)"
    | ConceptExpr.Exists(Term r, c) -> $"(exists {r} {printConcept c})"
    | ConceptExpr.All(Term r, c) -> $"(all {r} {printConcept c})"
    | ConceptExpr.Not c -> "¬" + printConcept c
    | ConceptExpr.Implies(l, r) -> $"(implies {printConcept l} {printConcept r})"
    | ConceptExpr.Equivalent(l, r) -> $"(equivalent {printConcept l} {printConcept r})"

let printDefinitions (definitions: TBox) =
    List.fold (fun state value -> state + printConcept value) "(definitions )" definitions
