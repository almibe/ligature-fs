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


let namedVariable (v: Variable) : bool =
    match v with
    | Variable "?" -> false
    | _ -> true

let elementPatternToValue (ep: ElementPattern): Value =
    match ep with
    | ElementPattern.Element e -> Value.Element e
    | ElementPattern.Variable v -> Value.Variable v

let testPattern
    ((elementPattern, attributePattern, valuePattern): Triple)
    ((element, attribute, value): Triple)
    : Map<Variable, Value> option =
    let mutable result = Map.empty
    let mutable isMatch = true

    match elementPattern with
    | ElementPattern.Variable variable ->
        if namedVariable variable then
            result <- Map.add variable (elementPatternToValue element) result
    | ElementPattern.Element elementP -> isMatch <- ElementPattern.Element elementP = element

    if isMatch then
        match attributePattern with
        | ElementPattern.Variable variable ->
            if namedVariable variable then
                if result.ContainsKey variable then
                    match result[variable] with
                    | Value.Element e -> isMatch <- ElementPattern.Element e = attribute
                    | _ -> failwith "TODO"
                else
                    result <- Map.add variable (elementPatternToValue attribute) result
        | ElementPattern.Element elementP -> isMatch <- attribute = ElementPattern.Element elementP

    if isMatch then
        match (valuePattern, value) with
        | (Value.Variable variable, value) ->
            if namedVariable variable then
                if result.ContainsKey variable then
                    match result[variable], value with
                    | Value.Element e, Value.Element v -> isMatch <- e = v
                    | Value.Literal l, Value.Literal v -> isMatch <- l = v
                    | _, _ -> isMatch <- false
                else
                    result <- Map.add variable value result
        | (Value.Element elementP, Value.Element value) -> isMatch <- elementP = value
        | (Value.Literal literal, Value.Literal value) -> isMatch <- literal = value
        | _ -> isMatch <- false

    if isMatch then Some result else None

let networkMatch (pattern: ElementPattern * ElementPattern * Value) (network: Network) : ResultSet =
    Set.fold
        (fun state entry ->
            match testPattern pattern entry with
            | Some res -> Set.add res state
            | None -> state)
        Set.empty
        network

let apply (pattern: Network) (resultSet: ResultSet) : Network =
    Set.map
        (fun (e, a, v) ->
            let element =
                match e with
                | ElementPattern.Element _ -> e
                | ElementPattern.Variable v -> failwith "TODO"

            let attribute =
                match a with
                | ElementPattern.Element _ -> a
                | ElementPattern.Variable v -> failwith "TODO"

            let value =
                match v with
                | Value.Element _ -> v
                | Value.Literal _ -> v
                | Value.Variable variable -> failwith "TODO"

            (element, attribute, value))
        pattern

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
