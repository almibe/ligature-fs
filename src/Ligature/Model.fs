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

type Element =
    { value: Term
      space: Term option
      langTag: Term option }

let element value space langTag =
    { value = value
      space = space
      langTag = langTag }

let el (value: string) =
    { value = Term value
      space = None
      langTag = None }

let termToElement (term: Term) : Element =
    { value = term
      space = None
      langTag = None }

type Triple = Element * Term * Element

and [<RequireQualifiedAccess>] Assertion =
    | Triple of Triple
    | Instance of Element * ConceptExpr
    | Same of Element * Element
    | Different of Element * Element

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
    // | Exactly of Term * ConceptExpr * int64
    // | AtLeast of Term * ConceptExpr * int64
    // | AtMost of Term * ConceptExpr * int64
    | Func of Term

// type INetwork =
//     abstract Triples: unit -> Async<Network>

and [<RequireQualifiedAccessAttribute>] Definition =
    | Implies of ConceptExpr * ConceptExpr
    | Equivalent of ConceptExpr * ConceptExpr

type ObjectView =
    { root: Element
      concepts: Set<ConceptExpr>
      links: Links }

and Links = Map<Term, List<ObjectView>>

type ElementNode =
    { root: Element
      concepts: Set<ConceptExpr>
      links: Map<Term, List<Element>> }

let objectView root concepts links : ObjectView =
    { root = root
      concepts = concepts
      links = links }

let emptyObjectView element : ObjectView =
    { root = element
      concepts = Set.empty
      links = Map.empty }

type Definitions = Set<Definition>

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
    abstract IsInstance: Term -> Element -> ConceptExpr -> Result<bool, LigatureError>
    abstract IsSubsumedBy: Term -> ConceptExpr -> ConceptExpr -> Result<bool, LigatureError>
    abstract IsEquivalent: Term -> ConceptExpr -> ConceptExpr -> Result<bool, LigatureError>
    abstract ReadElement: Term -> Element -> Result<ElementNode, LigatureError>
    abstract ReadElements: Term -> ConceptExpr -> Result<ElementNode seq, LigatureError>
    abstract Query: Term -> ConceptExpr -> Result<ObjectView seq, LigatureError>
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
            "and("
            conj
        + ")"
    | ConceptExpr.Or disj ->
        List.fold
            (fun state value ->
                if state = "" then
                    printConcept value
                else
                    state + $" {printConcept value}")
            "or("
            disj
        + ")"
    | ConceptExpr.Top -> "top()"
    | ConceptExpr.Bottom -> "bottom()"
    | ConceptExpr.Exists(Term r, c) -> $"exists({r} {printConcept c})"
    | ConceptExpr.All(Term r, c) -> $"all({r} {printConcept c})"
    | ConceptExpr.Not c -> $"not({printConcept c})"
    // | ConceptExpr.Implies(l, r) -> $"implies({printConcept l} {printConcept r})"
    // | ConceptExpr.Equivalent(l, r) -> $"equivalent({printConcept l} {printConcept r})"
    | ConceptExpr.Func(Term r) -> $"func({r})"

let printDefinition (definition: Definition) =
    match definition with
    | Definition.Implies(left, right) -> $"implies({printConcept left} {printConcept right})"
    | Definition.Equivalent(left, right) -> $"equivalent({printConcept left} {printConcept right})"

let printDefinitions (definitions: Definitions) =
    Set.fold (fun state value -> state + printDefinition value) "definitions(" definitions
    + ")"
