// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Main

type Symbol = string

type AtomicConcept = Symbol

type Concept = 
    | AtomicConcept of AtomicConcept
    | Disjuction of Disjunction
    | Conjuction of Conjunction
    | Equivalence of Equivalence
    | Not of Not
    | Subsumption of Subsumption

type Role = Symbol

type Subsumption = {subsumee: Concept, subsumer: Concept}

type Equivalence = {left: Concept, right: Concept, relation: "equalivant"}

type Not = { concept: Concept, relation: "not"}

type Conjuction = { left: Concept, right: Concept, relation: "conjunction" }

type Disjuction = { left: Concept, right: Concept, kind: "disjunction" }

type ExistentialRestriction = { concept: Concept, role: Role, kind: "existentialRestriction" }

type ValueRestriction = { concept: Concept, role: Role, kind: "valueRestriction" }

type Individual = {individual: Symbol, concept: Concept } 

type RoleInstance = { role: Role, left: Concept, right: Concept }

type ABox = Set<Individual | RoleInstance>

type TBox = Set<Concept>

// export function infer(tBox: TBox, aBox: ABox): ABox {
//     let results = Set()
//     let lastResults: undefined | Set<Individual> = undefined
//     let workingABox = aBox
//     while (results != lastResults) {
//         tBox.forEach((rule) => {
//             if (rule.subsumee && rule.subsumer) {
//                 const subConcept = rule.subsumee
//                 const concept = rule.subsumer
//                 workingABox.forEach(ind => {
//                     if (ind.individual && ind.concept == subConcept) {
//                         results = results.add(individual(ind.individual, concept))
//                     }
//                 })
//             } else if (rule.relation == "equalivant") {
//                 const left = rule.left
//                 const right = rule.right
//                 workingABox.forEach(ind => {
//                     if (ind.concept == left) {
//                         results = results.add(individual(ind.individual, right))
//                     } else if (ind.concept == right) {
//                         results = results.add(individual(ind.individual, left))
//                     }
//                 })
//             }
//             lastResults = results;
//             workingABox = results.union(workingABox);
//         })
//     }
//     return results.union(workingABox)
// }
