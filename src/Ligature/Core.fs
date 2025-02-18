// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Core

open Ligature.Model

let namedSlot (v: Slot) : bool =
    match v with
    | Slot "?" -> false
    | _ -> true

let testPattern
    ((elementPattern, attributePattern, valuePattern): TriplePattern)
    ((element, attribute, value): TriplePattern)
    : Map<Slot, TermPattern> option =
    let mutable result = Map.empty
    let mutable isMatch = true

    match elementPattern with
    | TermPattern.Slot slot ->
        if namedSlot slot then
            result <- Map.add slot (element) result
    | TermPattern.Term elementP -> isMatch <- TermPattern.Term elementP = element

    if isMatch then
        match attributePattern with
        | TermPattern.Slot slot ->
            if namedSlot slot then
                if result.ContainsKey slot then
                    match result[slot] with
                    | TermPattern.Term e -> isMatch <- TermPattern.Term e = attribute
                    | _ -> failwith "TODO"
                else
                    result <- Map.add slot (attribute) result
        | TermPattern.Term elementP -> isMatch <- attribute = TermPattern.Term elementP

    if isMatch then
        match (valuePattern, value) with
        | (TermPattern.Slot slot, value) ->
            if namedSlot slot then
                if result.ContainsKey slot then
                    match result[slot], value with
                    | TermPattern.Term e, TermPattern.Term v -> isMatch <- e = v
                    | _, _ -> isMatch <- false
                else
                    result <- Map.add slot value result
        | (TermPattern.Term elementP, TermPattern.Term value) -> isMatch <- elementP = value
        | _ -> isMatch <- false

    if isMatch then Some result else None

let singleMatch (pattern: TermPattern * TermPattern * TermPattern) (network: Pattern) : ResultSet =
    Set.fold
        (fun state entry ->
            match testPattern pattern entry with
            | Some res -> Set.add res state
            | None -> state)
        Set.empty
        network

let andSingleResult (left: Map<Slot, TermPattern>) (right: Map<Slot, TermPattern>) : Option<Map<Slot, TermPattern>> =
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

let networkMatch (pattern: Pattern) (network: Pattern) : ResultSet =
    let resultSets =
        Set.map (fun singlePattern -> singleMatch singlePattern network) pattern

    if resultSets.IsEmpty then
        Set.empty
    else
        List.reduce (fun state resultSet -> andResultSets state resultSet) (List.ofSeq resultSets)

let applyValueSet (pattern: Pattern) (result: ValueSet) : Pattern =
    Set.map
        (fun (e, a, v) ->
            let element =
                match e with
                | TermPattern.Term _ -> e
                | TermPattern.Slot v ->
                    if result.ContainsKey v then
                        match result[v] with
                        | TermPattern.Term e -> TermPattern.Term e
                        | TermPattern.Slot v -> TermPattern.Slot v
                    else
                        TermPattern.Slot v

            let attribute =
                match a with
                | TermPattern.Term _ -> a
                | TermPattern.Slot v ->
                    if result.ContainsKey v then
                        match result[v] with
                        | TermPattern.Term e -> TermPattern.Term e
                        | TermPattern.Slot v -> TermPattern.Slot v
                    else
                        TermPattern.Slot v

            let value =
                match v with
                | TermPattern.Term _ -> v
                | TermPattern.Slot slot ->
                    if result.ContainsKey slot then
                        result[slot]
                    else
                        TermPattern.Slot slot

            (element, attribute, value))
        pattern

// let applyValueSetQuoteTemplate (pattern: Quote) (result: ValueSet) : Quote =
//     List.map
//         (fun any ->
//             match any with
//             | Any.Slot slot ->
//                 if result.ContainsKey slot then
//                     match result[slot] with
//                     | TermPattern.Term e -> Any.Term e
//                     | TermPattern.Slot v -> Any.Slot v
//                 else
//                     Any.Slot slot
//             | _ -> any)
//         pattern

let apply (pattern: Pattern) (resultSet: ResultSet) : Pattern =
    Set.fold (fun state result -> Set.union (applyValueSet pattern result) state) Set.empty resultSet

let applySeq (pattern: Pattern) (resultSet: ResultSet) : Pattern list =
    Set.fold (fun state result -> (applyValueSet pattern result) :: state) [] resultSet

// let applySeqQuoteTemplate (pattern: Quote) (resultSet: ResultSet) : Quote list =
//     Set.fold (fun state result -> (applyValueSetQuoteTemplate pattern result) :: state) [] resultSet

let query (pattern: Pattern) (template: Pattern) (source: Pattern) : Pattern seq =
    let rs = networkMatch pattern source
    applySeq template rs

// let queryQuoteTemplate (pattern: Pattern) (template: Quote) (source: Pattern) : Quote seq =
//     let rs = networkMatch pattern source
//     applySeqQuoteTemplate template rs


let contains (test: Pattern) (source: Pattern) : bool = Set.isSubset test source

let filter (pattern: Pattern) (source: Pattern) : Pattern =
    let res = query pattern pattern source
    Seq.fold (fun state network -> Set.union state network) Set.empty res
