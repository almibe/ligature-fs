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
    | Inclusion of Inclusion
    | Defination of Definition
    | Disjunction of Disjunction
    | Conjunction of Conjunction
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

let entryToConceptExpression (input: Set<Entry>) : Set<ConceptExpression> = failwith "TODO"

let conceptExpressionToEntry (input: Set<ConceptExpression>) : Set<Entry> = failwith "TODO"

let infer (tBox: Set<ConceptExpression>) (aBox: Set<Entry>) : Result<Set<Entry>, TinyDLError> = failwith "TODO"

let top = Symbol "⊤"
let bottom = Symbol "⊥"
