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
    ((element, attribute, value): Triple)
    : ValueSet option =
    let mutable result: ValueSet = Map.empty
    let mutable isMatch = true

    match elementPattern with
    | TermPattern.Slot slot ->
        if namedSlot slot then
            result <- Map.add slot (Value.Term element) result
    | TermPattern.Term elementP -> isMatch <- elementP = element

    if isMatch then
        match attributePattern with
        | TermPattern.Slot slot ->
            if namedSlot slot then
                if result.ContainsKey slot then
                    match result.TryFind slot with
                    | Some a -> isMatch <- a = Value.Term attribute
                    | _ -> failwith "TODO"
                else
                    result <- Map.add slot (Value.Term attribute) result
        | TermPattern.Term attributeTerm -> isMatch <- attribute = attributeTerm

    if isMatch then
        match valuePattern with
        | ValuePattern.Slot slot ->
            if namedSlot slot then
                if result.ContainsKey slot then
                    match result.TryFind slot with
                    | Some vTerm -> isMatch <- vTerm = value
                    | _ -> isMatch <- false
                else
                    result <- Map.add slot value result
        | ValuePattern.Term valueTerm -> isMatch <- (Value.Term valueTerm) = value
        | _ -> isMatch <- false

    if isMatch then Some result else None

let singleMatch (pattern: TermPattern * TermPattern * ValuePattern) (network: Network) : ResultSet =
    Set.fold
        (fun state entry ->
            match testPattern pattern entry with
            | Some res -> Set.add res state
            | None -> state)
        Set.empty
        network

let andSingleResult (left: ValueSet) (right: ValueSet) : Option<ValueSet> =
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

let networkMatch (pattern: Pattern) (network: Network) : ResultSet =
    let resultSets =
        Set.map (fun singlePattern -> singleMatch singlePattern network) pattern

    if resultSets.IsEmpty then
        Set.empty
    else
        List.reduce (fun state resultSet -> andResultSets state resultSet) (List.ofSeq resultSets)

let applyValueSet (pattern: Pattern) (result: ValueSet) : Network =
    Set.map
        (fun (e, a, v) ->
            let element =
                match e with
                | TermPattern.Term t -> t
                | TermPattern.Slot v ->
                    if result.ContainsKey v then
                        match result.TryFind v with
                        | Some (Value.Term term) -> term
                        | _ -> failwith "Incomplete application."
                    else
                        failwith "Incomplete application."

            let attribute =
                match a with
                | TermPattern.Term t -> t
                | TermPattern.Slot v ->
                    match result.TryFind v with
                    | Some (Value.Term a) -> a
                    | _ -> failwith "Incomplete application."

            let value =
                match v with
                | ValuePattern.Term t -> Value.Term t
                | ValuePattern.Slot slot ->
                    match result.TryFind slot with
                    | Some t -> t
                    | None -> failwith "Incomplete application."

            element, attribute, value)
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

let apply (pattern: Pattern) (resultSet: ResultSet) : Network =
    Set.fold (fun state result -> Set.union (applyValueSet pattern result) state) Set.empty resultSet

let applySeq (pattern: Pattern) (resultSet: ResultSet) : Network list =
    Set.fold (fun state result -> (applyValueSet pattern result) :: state) [] resultSet

// let applySeqQuoteTemplate (pattern: Quote) (resultSet: ResultSet) : Quote list =
//     Set.fold (fun state result -> (applyValueSetQuoteTemplate pattern result) :: state) [] resultSet

let query (pattern: Pattern) (template: Pattern) (source: Network) : Network seq =
    let rs = networkMatch pattern source
    applySeq template rs

// let queryQuoteTemplate (pattern: Pattern) (template: Quote) (source: Pattern) : Quote seq =
//     let rs = networkMatch pattern source
//     applySeqQuoteTemplate template rs


let contains (test: Pattern) (source: Pattern) : bool = Set.isSubset test source

let filter (pattern: Pattern) (source: Network) : Network =
    let res = query pattern pattern source
    Seq.fold (fun state network -> Set.union state network) Set.empty res

let individuals (concept: Term) (tBox: Network) (aBox: Network) =
    
    failwith "TODO"
