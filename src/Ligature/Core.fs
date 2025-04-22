// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Core

open Ligature.Model

let namedSlot (v: Slot) : bool =
    match v with
    | Slot "?" -> false
    | _ -> true

let testPattern (pattern: AssertionPattern) (assertion: Assertion) : ValueSet option = failwith "TODO"
// let mutable result: ValueSet = Map.empty
// let mutable isMatch = true

// match assertion with
// | Assertion.Triple(element, attribute, value) ->
//     match elementPattern with
//     | TermPattern.Slot slot ->
//         if namedSlot slot then
//             result <- Map.add slot (Value.Term element) result
//     | TermPattern.Term elementP -> isMatch <- elementP = element

//     if isMatch then
//         match attributePattern with
//         | TermPattern.Slot slot ->
//             if namedSlot slot then
//                 if result.ContainsKey slot then
//                     match result.TryFind slot with
//                     | Some a -> isMatch <- a = Value.Term attribute
//                     | _ -> failwith "TODO"
//                 else
//                     result <- Map.add slot (Value.Term attribute) result
//         | TermPattern.Term attributeTerm -> isMatch <- attribute = attributeTerm

//     if isMatch then
//         match valuePattern with
//         | ValuePattern.Slot slot ->
//             if namedSlot slot then
//                 if result.ContainsKey slot then
//                     match result.TryFind slot with
//                     | Some vTerm -> isMatch <- vTerm = value
//                     | _ -> isMatch <- false
//                 else
//                     result <- Map.add slot value result
//         | ValuePattern.Term valueTerm -> isMatch <- (Value.Term valueTerm) = value
//         | _ -> isMatch <- false

//     if isMatch then Some result else None
// | Assertion.IsA(element, value) ->
//     failwith "TODO"

let singleMatch (pattern: AssertionPattern) (network: Assertions) : ResultSet =
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

let query (pattern: Pattern) (network: Assertions) : ResultSet =
    let resultSets =
        Set.map (fun singlePattern -> singleMatch singlePattern network) pattern

    if resultSets.IsEmpty then
        Set.empty
    else
        List.reduce (fun state resultSet -> andResultSets state resultSet) (List.ofSeq resultSets)

let applyValueSet (pattern: Pattern) (result: ValueSet) : Assertions = failwith "TODO"
// Set.map
//     (fun (e, a, v) ->
//         let element =
//             match e with
//             | TermPattern.Term t -> t
//             | TermPattern.Slot v ->
//                 if result.ContainsKey v then
//                     match result.TryFind v with
//                     | Some(Value.Term term) -> term
//                     | _ -> failwith "Incomplete application."
//                 else
//                     failwith "Incomplete application."

//         let attribute =
//             match a with
//             | TermPattern.Term t -> t
//             | TermPattern.Slot v ->
//                 match result.TryFind v with
//                 | Some(Value.Term a) -> a
//                 | _ -> failwith "Incomplete application."

//         let value =
//             match v with
//             | ValuePattern.Term t -> Value.Term t
//             | ValuePattern.Slot slot ->
//                 match result.TryFind slot with
//                 | Some t -> t
//                 | None -> failwith "Incomplete application."

//         Assertion.Triple(element, attribute, value))
//     pattern

// let applyValueSetTupleTemplate (pattern: Tuple) (result: ValueSet) : Tuple =
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

let apply (pattern: Pattern) (resultSet: ResultSet) : Assertions =
    Set.fold (fun state result -> Set.union (applyValueSet pattern result) state) Set.empty resultSet

let applySeq (pattern: Pattern) (resultSet: ResultSet) : Assertions list =
    Set.fold (fun state result -> (applyValueSet pattern result) :: state) [] resultSet

// let applySeqTupleTemplate (pattern: Tuple) (resultSet: ResultSet) : Tuple list =
//     Set.fold (fun state result -> (applyValueSetTupleTemplate pattern result) :: state) [] resultSet

// let queryTupleTemplate (pattern: Pattern) (template: Tuple) (source: Pattern) : Tuple seq =
//     let rs = networkMatch pattern source
//     applySeqTupleTemplate template rs


let contains (test: Pattern) (source: Pattern) : bool = Set.isSubset test source

let filter (pattern: Pattern) (source: Assertions) : Assertions =
    let res = query pattern source
    let res = applySeq pattern res
    Seq.fold (fun state network -> Set.union state network) Set.empty res

type ConceptValues = { isA: Set<Term>; isNot: Set<Term> }

type CompleteAlternative =
    { individuals: Map<Term, ConceptValues>
      roles: Map<Term * Term, Set<Term>>
      attributes: Map<Term * Literal, Set<Term>> }

type IncompleteAlternative =
    { assertions: Assertions
      individuals: Map<Term, ConceptValues>
      roles: Map<Term * Term, Set<Term>>
      attributes: Map<Term * Literal, Set<Term>> }

[<RequireQualifiedAccess>]
type Alternative =
    | Complete of CompleteAlternative
    | Incomplete of IncompleteAlternative

let newAlternative assertions =
    { assertions = assertions
      individuals = Map.empty
      roles = Map.empty
      attributes = Map.empty }

type Interpretation = Set<CompleteAlternative>

let addInstance
    (individuals: Map<Term, ConceptValues>)
    (individual: Term)
    (isA: Set<Term>)
    (isNot: Set<Term>)
    : Map<Term, ConceptValues> =
    match individuals.TryFind individual with
    | Some { isA = isA'; isNot = isNot' } ->
        Map.add
            individual
            { isA = Set.union isA' isA
              isNot = Set.union isNot' isNot }
            individuals
    | None -> Map.add individual { isA = isA; isNot = isNot } individuals

let addRole (interpretation: Interpretation) e a v =
    if interpretation.IsEmpty then
        failwith "TODO"
    else
        Set.map
            (fun
                { roles = roles
                  attributes = a
                  individuals = i } ->
                match roles.TryFind(e, v) with
                | Some value -> failwith "TODO"
                | None -> failwith "TODO")
            interpretation

let addAttribute interpretation e a l = failwith "TODO"

let interpretAssertion (assertion: Assertion) (alt: IncompleteAlternative) : Alternative list = failwith "TODO"
// let assertions = alt.assertions
// let mutable alt = alt

// Set.iter
//     (fun value ->
//         match value with
//         | Assertion.IsA(individual, ConceptExpr.AtomicConcept concept) ->
//             let assertions = Set.remove value alt.assertions
//             if assertions.IsEmpty then
//                 results <-
//                     List.append [ Alternative.Complete {
//                         individuals = addInstance alt.individuals individual (Set.ofList [ concept ]) Set.empty
//                         attributes = alt.attributes
//                         roles = alt.roles } ]
//                         results
//             else
//                 results <-
//                     List.append
//                         [ Alternative.Incomplete
//                             { assertions = assertions
//                               individuals = addInstance alt.individuals individual (Set.ofList [ concept ]) Set.empty
//                               attributes = alt.attributes
//                               roles = alt.roles } ] results
//         | Assertion.IsA(individual, ConceptExpr.Not(ConceptExpr.AtomicConcept concept)) ->
//             failwith "TODO"
//             // interpretation <- addInstance interpretation individual Set.empty (Set.ofList [ concept ])
//             // aBoxUnprocessed <- Set.remove value aBoxUnprocessed
//         | Assertion.IsA(individual, ConceptExpr.And group) ->
//             let mutable assertions = Set.remove value assertions
//             List.iter (fun expr ->
//                 assertions <- Set.add (Assertion.IsA(individual, expr)) assertions) group
//             if assertions.IsEmpty then
//                 results <-
//                     List.append [ Alternative.Complete {
//                         individuals = addInstance alt.individuals individual (Set.ofList [ concept ]) Set.empty
//                         attributes = alt.attributes
//                         roles = alt.roles } ]
//                         results
//             else
//                 results <-
//                     List.append
//                         [ Alternative.Incomplete
//                             { assertions = assertions
//                               individuals = addInstance alt.individuals individual (Set.ofList [ concept ]) Set.empty
//                               attributes = alt.attributes
//                               roles = alt.roles } ] results

//         | Assertion.IsA(individual, ConceptExpr.Or group) ->
//             failwith "TODO"
//             // List.iter (fun expr ->

//             //     failwith "TODO") group
//             // aBoxUnprocessed <- Set.remove value aBoxUnprocessed
//         | Assertion.IsA(_, _) -> failwith "TODO"
//         | Assertion.Triple(e, a, Value.Term v) ->
//             failwith "TODO"
//             //interpretation <- addRole interpretation e a v
//         | Assertion.Triple(e, a, Value.Literal l) ->
//             failwith "TODO")
//             //interpretation <- addAttribute interpretation e a l)
//     assertions
// results

let rec interpretABox (incomplete: IncompleteAlternative list) (complete: Interpretation) : Interpretation =
    match incomplete with
    | [] -> complete
    | alt :: tail ->
        let mutable complete = complete
        let mutable incomplete = tail
        let assertion = alt.assertions.MinimumElement

        let alt =
            { assertions = Set.remove assertion alt.assertions
              roles = alt.roles
              attributes = alt.attributes
              individuals = alt.individuals }

        List.iter
            (fun value ->
                match value with
                | Alternative.Complete newComlete -> complete <- Set.add newComlete complete
                | Alternative.Incomplete newIncomplete -> incomplete <- List.append [ newIncomplete ] incomplete)
            (interpretAssertion assertion alt)

        interpretABox incomplete complete

let rec interpret (tBox: Definitions) (aBox: Assertions) : Interpretation =
    let aBox' =
        if tBox.IsEmpty then
            aBox
        else
            Set.fold
                (fun state definition ->
                    match definition with
                    | Definition.Implies(a, c) ->
                        match c with
                        | ConceptExpr.AtomicConcept c ->
                            Set.fold
                                (fun state value ->
                                    match value with
                                    | Assertion.IsA(ind, ConceptExpr.AtomicConcept concept) when concept = a ->
                                        Set.add (Assertion.IsA(ind, ConceptExpr.AtomicConcept c)) state
                                    | _ -> state)
                                state
                                aBox
                        | ConceptExpr.And conj -> failwith "TODO"
                        | _ -> failwith "TODO"
                    | Definition.Define(a, c) -> failwith "TODO")
                aBox
                tBox

    if aBox <> aBox' then
        interpret tBox aBox'
    else
        interpretABox [ newAlternative aBox' ] Set.empty

let rec isConsistent (interpretation: Interpretation) : Result<bool, LigatureError> =
    Set.fold
        (fun
            state
            { roles = _
              attributes = _
              individuals = i } ->
            match state with
            | Ok false ->
                Map.fold
                    (fun state _ { isA = isA; isNot = isNot } ->
                        match state with
                        | Ok false ->
                            if (Set.intersect isA isNot).IsEmpty then
                                Ok true
                            else
                                Ok false
                        | e -> e)
                    (Ok false)
                    i
            | e -> e)
        (Ok false)
        interpretation

// if
//     query
//         (Set.ofList
//             [ TermPattern.Slot(Slot "?el"), TermPattern.Term(Term ":"), ValuePattern.Slot(Slot "?concept")
//               TermPattern.Slot(Slot "?el"), TermPattern.Term(Term "~:"), ValuePattern.Slot(Slot "?concept") ])
//         aBox
//     |> Seq.length = 0
// then
//     Ok true
// else
//     Ok false

// let mutable res = aBox

// Set.iter
//     (fun tStatement ->
//         Set.iter
//             (fun aStatement ->
//                 match tStatement, aStatement with
//                 | Definition.Subconcept (subconcept, AtomicConcept superconcept), (element, Term ":", Value.Term concept) when
//                     subconcept = concept
//                     ->
//                     res <- Set.add (element, Term ":", Value.Term superconcept) res
//                 // | (firstRole, Term "tiny-dl.inverse-of", Value.Term secondRole), (first, role, Value.Term second) when
//                 //     role = firstRole
//                 //     ->
//                 //     res <- Set.add (second, secondRole, Value.Term first) res
//                 // | (firstRole, Term "tiny-dl.inverse-of", Value.Term secondRole), (first, role, Value.Term second) when
//                 //     role = secondRole
//                 //     ->
//                 //     res <- Set.add (second, firstRole, Value.Term first) res
//                 // | (roleName, Term ":", Value.Term(Term "tiny-dl.Is-Symmetrical")),
//                 //   (first, role, Value.Term second) when role = roleName ->
//                 //     res <- Set.add (second, role, Value.Term first) res
//                 | _ -> ())
//             aBox)
//     tBox

// if aBox = res then Ok res else infer tBox res
