// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Main

type Symbol = string

type AtomicConcept = Symbol

let top: AtomicConcept = "⊤"

let bottom: AtomicConcept = "⊥"

type Role = Symbol

type Concept =
    | AtomicConcept of AtomicConcept
    | Disjunction of Disjunction
    | Conjunction of Conjunction
    | Equivalence of Equivalence
    | Not of Not
    | Subsumption of Subsumption

and Subsumption =
    { subsumee: Concept; subsumer: Concept }

and Equivalence = { left: Concept; right: Concept }

and Not = { concept: Concept }

and Conjunction = { left: Concept; right: Concept }

and Disjunction = { left: Concept; right: Concept }

and ExistentialRestriction = { concept: Concept; role: Role }

and ValueRestriction = { concept: Concept; role: Role }

and UnaryPredicate = { symbol: Symbol; concept: Concept }

and BinaryPredicate =
    { role: Role
      left: Symbol
      right: Symbol }

and ABoxValues =
    | UnaryPredicate of UnaryPredicate
    | BinaryPredicate of BinaryPredicate

and ABox = Set<ABoxValues>

and TBox = Set<Concept>

let check (tBox: TBox) (aBox: ABox) : bool = true

let infer (tBox: TBox) (aBox: ABox) : ABox = Set.empty
