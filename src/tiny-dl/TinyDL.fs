// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Main

open Ligature.Main

type TinyDLError = string

type AtomicConcept = Element

type Role = Element

and ConceptExpression =
    | AtomicConcept of AtomicConcept
    // | Disjunction of Disjunction
    //| Conjunction of Conjunction
    | Not of Not

and KnowledgeBase = Set<Entry> * Set<ConceptExpression>

and [<RequireQualifiedAccess>] NormalConceptExpression =
    | AtomicConcept of AtomicConcept
    | Not of AtomicConcept

and Inclusion =
    { left: AtomicConcept
      right: ConceptExpression }

and Definition =
    { left: AtomicConcept
      right: ConceptExpression }

and Not = { concept: ConceptExpression }

and Conjunction = { expressions: ConceptExpression }

and Disjunction =
    { left: ConceptExpression
      right: ConceptExpression }

and ExistentialRestriction =
    { concept: ConceptExpression
      role: Role }

and ValueRestriction =
    { concept: ConceptExpression
      role: Role }


// type TermEntry =
//     | AtomicConcept of Element
//     | Equiv of Element * Element
//     | SubConcept of Element * Element

let entryToTermEntry (input: Set<Entry>): Set<ConceptExpression> = failwith "TODO"

let termEntryToEntry (input: Set<ConceptExpression>): Set<Entry> = failwith "TODO"

let infer (tBox: Set<ConceptExpression>) (aBox: Set<Entry>) : Result<Set<Entry>, TinyDLError> =
    failwith "TODO"

type Interpretation =
    { Domain: Set<Element>
      Concepts: Map<Element, Set<Element>>
      Roles: Map<Element, Set<Element * Element>> }

let top = Symbol "⊤"
let bottom = Symbol "⊥"

let interpret tBox aBox : Result<Interpretation, TinyDLError> =
    failwith "TODO"

// let eval (script: string) : Result<Interpretation, TinyDLError> =
//     match tokenize script with
//     | Ok res ->
//         match parse res with
//         | Ok res -> failwith "TODO" //interpret res
//         | Error errorValue -> Error errorValue
//     | Error errorValue -> Error errorValue
