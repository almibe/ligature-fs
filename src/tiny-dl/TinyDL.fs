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

and Ontology = Set<ConceptExpression>

and KnowledgeBase = Network * Set<ConceptExpression>

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

let networkToOntology (input: Network) : Ontology =
    let entryToOntology (entry: Entry) : ConceptExpression =
        match entry with
        | Role { first = first
                 second = second
                 role = Symbol "subconcept" } ->
            Inclusion
                { left = first
                  right = AtomicConcept second }
        | _ -> failwith "TODO"

    Set.map entryToOntology input

let ontologyToNetwork (input: Ontology) : Network = failwith "TODO"

let infer (ontology: Ontology) (network: Network) : Result<Network, TinyDLError> =
    let mutable result = network

    Set.iter
        (fun expression ->
            match expression with
            | Inclusion { left = left
                          right = AtomicConcept right } ->
                Set.iter
                    (fun entry ->
                        match entry with
                        | Extension { element = element; concept = concept } ->
                            if left = concept then
                                result <- Set.add (Extension { element = element; concept = right }) result
                        | _ -> ())
                    network
            | _ -> failwith "TODO")
        ontology

    Ok result

let top = Symbol "⊤"
let bottom = Symbol "⊥"
