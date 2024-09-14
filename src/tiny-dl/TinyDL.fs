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

and Role = Symbol

and Subsumption =
    { subsumee: Concept; subsumer: Concept }

and Equivalence = { left: Concept; right: Concept }

and Not = { concept: Concept }

and Conjunction = { left: Concept; right: Concept }

and Disjunction = { left: Concept; right: Concept }

and ExistentialRestriction = { concept: Concept; role: Role }

and ValueRestriction = { concept: Concept; role: Role }

and UnaryPredicate = { symbol: Symbol; concept: Concept }

and BinaryConceptPredicate =
    { role: Role
      left: Concept
      right: Concept }

and BinarySymbolPredicate =
    { role: Role
      left: Symbol
      right: Symbol }

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
