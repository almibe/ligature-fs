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

type Slot = Slot of string

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

[<RequireQualifiedAccess>]
type Assertion =
    | Triple of Element * Term * Element
    | Instance of Element * ConceptExpr

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
    | Func of Term * ConceptExpr
    | Nominal of Element

// type INetwork =
//     abstract Triples: unit -> Async<Network>

and [<RequireQualifiedAccessAttribute>] Definition =
    | Implies of ConceptExpr * ConceptExpr
    | Equivalent of ConceptExpr * ConceptExpr

and [<RequireQualifiedAccess>] PatternPart =
    | Triple of ElementPattern * TermPattern * ElementPattern
    | Instance of ElementPattern * ConceptExpr

and Pattern = Set<PatternPart>

and [<RequireQualifiedAccess>] TermPattern =
    | Term of Term
    | Slot of Slot

and [<RequireQualifiedAccess>] ElementPattern =
    | Element of Element
    | Slot of Slot

type Definitions = Set<Definition>

type KnowledgeBase = Definitions * Assertions

type ResultSet = Set<Map<Slot, Element>>

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
    abstract Instances: Term -> ConceptExpr -> Result<Element seq, LigatureError>
    abstract Query: Term -> Pattern -> Result<ResultSet, LigatureError>
    abstract TableauModels: Term -> Result<Set<Assertions>, LigatureError>

let encodeString string =
#if !FABLE_COMPILER
    System.Web.HttpUtility.JavaScriptStringEncode(string, true)
#else
    Fable.Core.JsInterop.emitJsExpr string "JSON.stringify($0)"
#endif

let printElement (element: Element) : string =
    match element with
    | { value = Term l
        space = Some(Term t)
        langTag = Some(Term langTag) } -> $"element({encodeString l} {encodeString t} {encodeString langTag})"
    | { value = Term l
        space = None
        langTag = None } -> encodeString l


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
    | ConceptExpr.Func(Term r, c) -> $"func({r} {printConcept c})"
    | ConceptExpr.Nominal e -> $"nominal({printElement e})"

let printDefinition (definition: Definition) =
    match definition with
    | Definition.Implies(left, right) -> $"implies({printConcept left} {printConcept right})"
    | Definition.Equivalent(left, right) -> $"equivalent({printConcept left} {printConcept right})"

let printDefinitions (definitions: Definitions) =
    Set.fold (fun state value -> state + printDefinition value) "definitions(" definitions
    + ")"
