// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Model

open Ligature.Model

type TinyDLError = string

and AtomicConcept = Element

and Role = Element

and Expression =
    | Subconcept of Subconcept
    | Equivalent of AtomicConcept * AtomicConcept

and Description = Set<Expression>

and [<RequireQualifiedAccess>] NormalConceptExpression =
    | AtomicConcept of Element
    | Not of AtomicConcept

and Subconcept =
    { superconcept: AtomicConcept
      subconcept: AtomicConcept }

let networkToDescription (input: Network) : Description =
    let entryToDescription (triple: Triple) : Expression =
        match triple with
        | (ElementPattern.Element sub, ElementPattern.Element(Element "subconcept-of"), Value.Element super) ->
            Subconcept
                { subconcept = sub
                  superconcept = super }
        | _ -> failwith "TODO"

    Set.map entryToDescription input

let descriptionToNetwork (input: Description) : Network = failwith "TODO"

let infer (description: Description) (network: Network) : Result<Network, TinyDLError> =
    let mutable result = network

    Set.iter
        (fun expression ->
            match expression with
            | Subconcept { subconcept = sub
                           superconcept = super } ->
                Set.iter
                    (fun entry ->
                        match entry with
                        | element, ElementPattern.Element(Element ":"), concept ->
                            match concept with
                            | Value.Element e ->
                                if sub = e then
                                    result <-
                                        Set.add
                                            (element, ElementPattern.Element(Element ":"), Value.Element super)
                                            result
                            | _ -> failwith "TODO"
                        | _ -> ())
                    network
            | _ -> failwith "TODO")
        description

    Ok result

let top = Element "⊤"
let bottom = Element "⊥"
