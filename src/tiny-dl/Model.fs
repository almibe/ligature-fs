// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Model

open Ligature.Model

type TinyDLError = string

and AtomicConcept = Element

and Role = Element

and Description =
    | Subconcept of Subconcept
    | Equivalent of AtomicConcept * AtomicConcept

// and ConceptExpression =
//     | AtomicConcept of ConceptName
//     | Inclusion of Inclusion
//     | Defination of Definition
//     | Disjunction of Disjunction
//     | Conjunction of Conjunction
//     | Not of Not

//and Description = Set<ConceptExpression>

//and Script = Description * Network * Network

and [<RequireQualifiedAccess>] NormalConceptExpression =
    | AtomicConcept of Element
    | Not of AtomicConcept

and Subconcept =
    { superconcept: AtomicConcept
      subconcept: AtomicConcept }

// and Definition =
//     { left: AtomicConcept
//       right: ConceptExpression }

// and Not = { concept: ConceptExpression }

// and Conjunction = { expressions: ConceptExpression }

// and Disjunction =
//     { left: ConceptExpression
//       right: ConceptExpression }

// and ExistentialRestriction =
//     { concept: ConceptExpression
//       role: Role }

// and ValueRestriction =
//     { concept: ConceptExpression
//       role: Role }

let networkToDescription (input: Network) : Description = failwith "TODO"
// let entryToDescription (entry: Triple) : ConceptExpression =
//     match entry with
//     // | Entry.Role { first = first
//     //                second = second
//     //                role = Element "subconcept" } ->
//     //     Inclusion
//     //         { left = first
//     //           right = AtomicConcept second }
//     | _ -> failwith "TODO"

// Set.map entryToDescription input

let descriptionToNetwork (input: Description) : Network = failwith "TODO"

let infer (description: Description) (network: Network) : Result<Network, TinyDLError> =
    let mutable result = network

    Set.iter
        (fun expression ->
            match expression with
            | Inclusion { left = left
                          right = AtomicConcept right } ->
                Set.iter
                    (fun entry ->
                        match entry with
                        // | Entry.Extends { element = element; concept = concept } ->
                        //     if left = concept then
                        //         result <- Set.add (Entry.Extends { element = element; concept = right }) result
                        | _ -> ())
                    network
            | _ -> failwith "TODO")
        description

    Ok result

let top = Element "⊤"
let bottom = Element "⊥"
