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

let elementPatternToValue (ep: ElementPattern) : Value =
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

let singleMatch (pattern: ElementPattern * ElementPattern * Value) (network: Network) : ResultSet =
    Set.fold
        (fun state entry ->
            match testPattern pattern entry with
            | Some res -> Set.add res state
            | None -> state)
        Set.empty
        network

let andSingleResult (left: Map<Variable, Value>) (right: Map<Variable, Value>) : Option<Map<Variable, Value>> =
    let leftKeys = Set.ofSeq left.Keys
    let rightKeys = Set.ofSeq right.Keys
    let intersection = Set.intersect leftKeys rightKeys

    let res =
        Set.forall (fun sharedKey -> left[sharedKey] = right[sharedKey]) intersection

    if res then
        Map.fold (fun state key value -> Map.add key value state) left right |> Some
    else
        None

let andResultSets (left: ResultSet) (right: ResultSet) : ResultSet =
    let mutable result = Set.empty

    Set.iter
        (fun leftResult ->
            Set.iter
                (fun rightRight ->
                    match andSingleResult leftResult rightRight with
                    | Some res -> result <- Set.add res result
                    | None -> ())
                right)
        left

    result

let networkMatch (pattern: Network) (network: Network) : ResultSet =
    let resultSets =
        Set.map (fun singlePattern -> singleMatch singlePattern network) pattern

    if resultSets.IsEmpty then
        Set.empty
    else
        List.reduce (fun state resultSet -> andResultSets state resultSet) (List.ofSeq resultSets)

let applySingleResult (pattern: Network) (result: Map<Variable, Value>) : Network =
    Set.map
        (fun (e, a, v) ->
            let element =
                match e with
                | ElementPattern.Element _ -> e
                | ElementPattern.Variable v ->
                    if result.ContainsKey v then
                        match result[v] with
                        | Value.Element e -> ElementPattern.Element e
                        | Value.Literal l -> failwith "illegal value"
                        | Value.Variable v -> ElementPattern.Variable v
                    else
                        ElementPattern.Variable v

            let attribute =
                match a with
                | ElementPattern.Element _ -> a
                | ElementPattern.Variable v ->
                    if result.ContainsKey v then
                        match result[v] with
                        | Value.Element e -> ElementPattern.Element e
                        | Value.Literal l -> failwith "illegal value"
                        | Value.Variable v -> ElementPattern.Variable v
                    else
                        ElementPattern.Variable v

            let value =
                match v with
                | Value.Element _ -> v
                | Value.Literal _ -> v
                | Value.Variable variable ->
                    if result.ContainsKey variable then
                        result[variable]
                    else
                        Value.Variable variable

            (element, attribute, value))
        pattern

let apply (pattern: Network) (resultSet: ResultSet) : Network =
    Set.fold (fun state result -> Set.union (applySingleResult pattern result) state) Set.empty resultSet

let query (pattern: Network) (template: Network) (source: Network) : Network =
    let rs = networkMatch pattern source
    apply template rs

let contains (test: Network) (source: Network) : bool = Set.isSubset test source

let filter (pattern: Network) (source: Network) : Network = query pattern pattern source
