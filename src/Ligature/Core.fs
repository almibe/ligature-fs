// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Core

open Ligature.Model

let namedSlot (v: Variable) : bool =
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
    | ElementPattern.Variable slot ->
        if namedSlot slot then
            result <- Map.add slot (elementPatternToValue element) result
    | ElementPattern.Element elementP -> isMatch <- ElementPattern.Element elementP = element

    if isMatch then
        match attributePattern with
        | ElementPattern.Variable slot ->
            if namedSlot slot then
                if result.ContainsKey slot then
                    match result[slot] with
                    | Value.Element e -> isMatch <- ElementPattern.Element e = attribute
                    | _ -> failwith "TODO"
                else
                    result <- Map.add slot (elementPatternToValue attribute) result
        | ElementPattern.Element elementP -> isMatch <- attribute = ElementPattern.Element elementP

    if isMatch then
        match (valuePattern, value) with
        | (Value.Variable slot, value) ->
            if namedSlot slot then
                if result.ContainsKey slot then
                    match result[slot], value with
                    | Value.Element e, Value.Element v -> isMatch <- e = v
                    | Value.Literal l, Value.Literal v -> isMatch <- l = v
                    | _, _ -> isMatch <- false
                else
                    result <- Map.add slot value result
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

let applyValueSet (pattern: Network) (result: ValueSet) : Network =
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
                | Value.Variable slot ->
                    if result.ContainsKey slot then
                        result[slot]
                    else
                        Value.Variable slot
            (element, attribute, value))
        pattern

let applyValueSetQuoteTemplate (pattern: Quote) (result: ValueSet) : Quote =
    List.map
        (fun any ->
            match any with
            | Any.Variable slot ->
                if result.ContainsKey slot then
                    match result[slot] with
                    | Value.Element e -> Any.Element e
                    | Value.Literal l -> Any.Literal l
                    | Value.Variable v -> Any.Variable v
                else
                    Any.Variable slot
            | _ -> any)
        pattern

let apply (pattern: Network) (resultSet: ResultSet) : Network =
    Set.fold (fun state result -> Set.union (applyValueSet pattern result) state) Set.empty resultSet

let applySeq (pattern: Network) (resultSet: ResultSet) : Network list =
    Set.fold (fun state result -> (applyValueSet pattern result) :: state) [] resultSet

let applySeqQuoteTemplate (pattern: Quote) (resultSet: ResultSet) : Quote list =
    Set.fold (fun state result -> (applyValueSetQuoteTemplate pattern result) :: state) [] resultSet

let query (pattern: Network) (template: Network) (source: Network) : Network seq =
    let rs = networkMatch pattern source
    applySeq template rs

let queryQuoteTemplate (pattern: Network) (template: Quote) (source: Network) : Quote seq =
    let rs = networkMatch pattern source
    applySeqQuoteTemplate template rs


let contains (test: Network) (source: Network) : bool = Set.isSubset test source

let filter (pattern: Network) (source: Network) : Network =
    let res = query pattern pattern source
    Seq.fold (fun state network -> Set.union state network) Set.empty res
