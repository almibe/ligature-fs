// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Main

type TinyDLError = string

type Symbol = string

type AtomicConcept = Symbol

type Role = Symbol

type Term =
    | Definition of Definition
    | Inclusion of Inclusion

and ConceptExpression =
    | AtomicConcept of AtomicConcept
    | Disjunction of Disjunction
    | Conjunction of Conjunction
    | Not of Not

and Inclusion = { left: AtomicConcept; right: ConceptExpression }

and Definition = { left: AtomicConcept; right: ConceptExpression }

and Not = { concept: ConceptExpression }

and Conjunction = { left: ConceptExpression; right: ConceptExpression }

and Disjunction = { left: ConceptExpression; right: ConceptExpression }

and ExistentialRestriction = { concept: ConceptExpression; role: Role }

and ValueRestriction = { concept: ConceptExpression; role: Role }

and UnaryPredicate =
    { symbol: Symbol
      concept: AtomicConcept }

and BinaryPredicate =
    { role: Role
      left: Symbol
      right: Symbol }

and ABoxValue =
    | UnaryPredicate of UnaryPredicate
    | BinaryPredicate of BinaryPredicate

and ABox = Set<ABoxValue>

and TBox = Set<Term>

and KnowledgeBase = TBox * ABox

and Interpretation =
    { Domain: Set<Symbol>
      Concepts: Map<Symbol, Set<Symbol>>
      Roles: Map<Symbol, Set<Symbol * Symbol>> }

let top: AtomicConcept = "⊤"
let bottom: AtomicConcept = "⊥"
let emptyABox: ABox = Set.empty
let emptyTBox: TBox = Set.empty
let emptyKB: KnowledgeBase = Set.empty, Set.empty
