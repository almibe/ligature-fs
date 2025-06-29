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

type Instance =
    { value: string
      space: Term option
      langTag: string option }

let termToInstance (Term term) : Instance =
    { value = term
      space = None
      langTag = None }

type Slot = Slot of string

type ResultSet = Set<ValueSet>

and ValueSet = Map<Slot, Instance>

and [<RequireQualifiedAccess>] TermPattern =
    | Term of Term
    | Slot of Slot

and [<RequireQualifiedAccess>] ValuePattern =
    | Term of Term
    | Instance of Instance
    | Slot of Slot

and Triple = Instance * Term * Instance

and [<RequireQualifiedAccess>] Assertion =
    | Triple of Triple
    | Instance of Instance * ConceptExpr
    | Same of Instance * Instance
    | Different of Instance * Instance

and Assertions = Set<Assertion>

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
    | Func of Term

// type INetwork =
//     abstract Triples: unit -> Async<Network>

type Definitions = Set<ConceptExpr>

type KnowledgeBase = Definitions * Assertions

type ILigatureStore =
    abstract KBs: unit -> Term seq
    abstract AddKB: Term -> unit
    abstract RemoveKB: Term -> unit
    abstract AssertKB: Term -> Assertions -> unit
    abstract UnassertKB: Term -> Assertions -> unit
    abstract ReadAssertsKB: Term -> Result<Assertions, LigatureError>
    abstract DefineKB: Term -> Definitions -> unit
    abstract UndefineKB: Term -> Definitions -> unit
    abstract ReadDefinitionsKB: Term -> Result<Definitions, LigatureError>
    abstract ReadKB: Term -> Result<KnowledgeBase, LigatureError>
    abstract IsConsistent: Term -> Result<bool, LigatureError>
    abstract IsSatisfiable: Term -> ConceptExpr -> Result<bool, LigatureError>
    abstract IsInstance: Term -> Instance -> ConceptExpr -> Result<bool, LigatureError>
    abstract IsSubsumedBy: Term -> ConceptExpr -> ConceptExpr -> Result<bool, LigatureError>
    abstract IsEquivalent: Term -> ConceptExpr -> ConceptExpr -> Result<bool, LigatureError>
    abstract Query: Term -> ConceptExpr -> Result<Instance, LigatureError>
    abstract TableauModels: Term -> Result<Set<Assertions>, LigatureError>

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
    | ConceptExpr.Func r -> failwith "TODO"

let printDefinitions (definitions: Definitions) =
    Set.fold (fun state value -> state + printConcept value) "definitions (" definitions
    + ")"
