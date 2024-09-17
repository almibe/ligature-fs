// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Main

type Symbol = string

type AtomicConcept = Symbol

type Role = Symbol

type Name =
    | AtomicConcept of AtomicConcept
    | Role of Role

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

and ABoxValue =
    | UnaryPredicate of UnaryPredicate
    | BinaryPredicate of BinaryPredicate

and ABox = Set<ABoxValue>

and TBox = Set<Concept>

and KnowledgeBase = TBox * ABox

and Interpretation = Map<Name, Set<Symbol>>

let top: AtomicConcept = "⊤"
let bottom: AtomicConcept = "⊥"
let emptyABox: ABox = Set.empty
let emptyTBox: TBox = Set.empty
let emptyKB: KnowledgeBase = Set.empty, Set.empty

let interpret ((tBox, aBox): KnowledgeBase) : Interpretation =
    Set.fold (fun state entry -> 
        match entry with
        | UnaryPredicate up ->
            match up with
            | { symbol = symbol; concept = AtomicConcept(concept) } ->
                Map.add (Name.AtomicConcept concept) (Set.ofList [symbol]) state
            | _ -> failwith "TODO"
        | BinaryPredicate bp -> failwith "TODO") Map.empty aBox
