// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Core

open Ligature.Model

// let isComplete (entries: Set<Entry>) : bool =
//     let concepts =
//         Set.fold
//             (fun state value ->
//                 match value with
//                 | Entry.Extends { concept = concept } -> Set.add concept state
//                 | _ -> state)
//             Set.empty
//             entries

//     Set.fold
//         (fun state (entry: Entry) ->
//             match entry with
//             //            | Entry.Attribute { first = first; second = second } -> (concepts.Contains first) && (concepts.Contains second)
//             | _ -> state)
//         true
//         entries

// let isConsistent (network: Network) : bool =
//     let mutable individuals: Map<Element, Set<Entry>> = Map.empty

//     Set.fold
//         (fun state (entry: Entry) ->
//             match state with
//             | false -> false
//             | true ->
//                 match entry with
//                 | Entry.Extends { concept = conceptName
//                                   element = symbol } ->
//                     let concept =
//                         Entry.Extends
//                             { concept = conceptName
//                               element = symbol }

//                     let notVersion =
//                         Entry.NotExtends
//                             { concept = conceptName
//                               element = symbol }

//                     match individuals.TryFind symbol with
//                     | None ->
//                         individuals <- Map.add symbol (Set.ofList [ concept ]) individuals
//                         true
//                     | Some res ->
//                         if res.Contains(notVersion) then
//                             false
//                         else
//                             individuals <- Map.add symbol (Set.add (concept) res) individuals
//                             true
//                 | Entry.NotExtends { concept = concept; element = symbol } ->
//                     let notConcept = Entry.NotExtends { concept = concept; element = symbol }
//                     let inverse = Entry.Extends { concept = concept; element = symbol }

//                     match individuals.TryFind symbol with
//                     | None ->
//                         individuals <- Map.add symbol (Set.ofList [ notConcept ]) individuals
//                         true
//                     | Some res ->
//                         if res.Contains(inverse) then
//                             false
//                         else
//                             individuals <- Map.add symbol (Set.add notConcept res) individuals
//                             true
//                 | Entry.Attribute _ -> true)
//         true
//         network

let testPattern
    ((elementPattern, attributePattern, valuePattern): EntryPattern)
    ((element, attribute, value): Entry)
    : Map<Variable, Value> option =
    let mutable result = Map.empty
    let mutable isMatch = true

    match elementPattern with
    | ElementPattern.Variable variable -> result <- Map.add variable (Value.Element element) result
    | ElementPattern.Element elementP -> isMatch <- elementP = element

    if isMatch then
        match attributePattern with
        | ElementPattern.Variable variable -> result <- Map.add variable (Value.Element attribute) result
        | ElementPattern.Element elementP -> isMatch <- attribute = elementP

    if isMatch then
        match (valuePattern, value) with
        | (ValuePattern.Variable variable, _) -> result <- Map.add variable value result
        | (ValuePattern.Element elementP, Value.Element value) -> isMatch <- elementP = value
        | (ValuePattern.Literal literal, Value.Literal value) -> isMatch <- literal = value
        | _ -> isMatch <- false

    if isMatch then Some result else None

let networkMatch (pattern: ElementPattern * ElementPattern * ValuePattern) (network: Network) : ResultSet =
    Set.fold
        (fun state entry ->
            match testPattern pattern entry with
            | Some res -> Set.add res state
            | None -> state)
        Set.empty
        network

// let contains (test: Network) (source: Network) : bool =
//     Set.isSubset test source

// let matches (pattern: Pattern) (source: Network) : bool =
//     if pattern.IsEmpty then
//         true
//     else
//         // let matches = matchEach pattern source

//         failwith "TODO"

// let find (pattern: Pattern) (source: Network) : Set<Network> = failwith "TODO"

// let filter (pattern: Pattern) (source: Network) : Network =
//     failwith "TODO"
//     // if pattern.IsEmpty then
//     //     Set.empty
//     // else
//     //     let mutable results = Set.empty
//     //     Set.iter
//     //         (fun root ->
//     //             Set.iter (fun pattern ->
//     //                 failwith "TODO")
//     //                 pattern
//     //             failwith "TODO")
//     //         source
//     //     results
