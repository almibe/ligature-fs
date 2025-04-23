// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Interpreter

open Ligature.Model

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

type InterpretationStep =
    { current: IncompleteAlternative
      incomplete: List<IncompleteAlternative>
      clash: Set<CompleteAlternative> }

[<RequireQualifiedAccess>]
type InterpretationStepResult =
    | NextStep of InterpretationStep
    | Model of CompleteAlternative

type Interpretation = CompleteAlternative option

let newInterpretation assertions : InterpretationStep =
    { current = newAlternative assertions
      incomplete = []
      clash = Set.empty }

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

let addRole (interpretation: InterpretationStep) e a v = failwith "TODO"
// if interpretation.IsEmpty then
//     failwith "TODO"
// else
//     Set.map
//         (fun
//             { roles = roles
//               attributes = a
//               individuals = i } ->
//             match roles.TryFind(e, v) with
//             | Some value -> failwith "TODO"
//             | None -> failwith "TODO")
//         interpretation

let addAttribute interpretation e a l = failwith "TODO"

let interpretAssertion (assertion: Assertion) (step: InterpretationStep) : InterpretationStepResult =
    match assertion with
    | Assertion.IsA(individual, ConceptExpr.AtomicConcept concept) ->
        let assertions = Set.remove assertion step.current.assertions

        if assertions.IsEmpty then
            InterpretationStepResult.Model(
                { individuals = addInstance step.current.individuals individual (Set.ofList [ concept ]) Set.empty
                  attributes = step.current.attributes
                  roles = step.current.roles }
            )
        else
            InterpretationStepResult.NextStep
                { current =
                    { assertions = assertions
                      individuals = addInstance step.current.individuals individual (Set.ofList [ concept ]) Set.empty
                      attributes = step.current.attributes
                      roles = step.current.roles }
                  clash = step.clash
                  incomplete = step.incomplete }
    | Assertion.IsA(individual, ConceptExpr.Not(ConceptExpr.AtomicConcept concept)) ->
        let assertions = Set.remove assertion step.current.assertions

        if assertions.IsEmpty then
            InterpretationStepResult.Model(
                { individuals = addInstance step.current.individuals individual Set.empty (Set.ofList [ concept ])
                  attributes = step.current.attributes
                  roles = step.current.roles }
            )
        else
            InterpretationStepResult.NextStep
                { current =
                    { assertions = assertions
                      individuals = addInstance step.current.individuals individual Set.empty (Set.ofList [ concept ])
                      attributes = step.current.attributes
                      roles = step.current.roles }
                  clash = step.clash
                  incomplete = step.incomplete }
    | Assertion.IsA(individual, ConceptExpr.And group) -> failwith "TODO"
    // let mutable assertions = Set.remove value assertions
    // List.iter (fun expr ->
    //     assertions <- Set.add (Assertion.IsA(individual, expr)) assertions) group
    // if assertions.IsEmpty then
    //     results <-
    //         List.append [ Alternative.Complete {
    //             individuals = addInstance alt.individuals individual (Set.ofList [ concept ]) Set.empty
    //             attributes = alt.attributes
    //             roles = alt.roles } ]
    //             results
    // else
    //     results <-
    //         List.append
    //             [ Alternative.Incomplete
    //                 { assertions = assertions
    //                     individuals = addInstance alt.individuals individual (Set.ofList [ concept ]) Set.empty
    //                     attributes = alt.attributes
    //                     roles = alt.roles } ] results

    | Assertion.IsA(individual, ConceptExpr.Or group) -> failwith "TODO"
    // List.iter (fun expr ->

    //     failwith "TODO") group
    // aBoxUnprocessed <- Set.remove value aBoxUnprocessed
    | Assertion.IsA(_, _) -> failwith "TODO"
    | Assertion.Triple(e, a, Value.Term v) -> failwith "TODO"
    //interpretation <- addRole interpretation e a v
    | Assertion.Triple(e, a, Value.Literal l) -> failwith "TODO"
//interpretation <- addAttribute interpretation e a l)

let rec interpretABox (interpretation: InterpretationStep) : Interpretation =
    if interpretation.current.assertions.IsEmpty then
        match interpretation.incomplete with
        | [] -> None
        | _ -> failwith "TODO"
    else
        let assertion = interpretation.current.assertions.MinimumElement

        match interpretAssertion assertion interpretation with
        | InterpretationStepResult.Model model -> Some model
        | InterpretationStepResult.NextStep step -> interpretABox step
// if interpretation.incomplete.IsEmpty then
//     toInterpretation interpretation
// else
//     let mutable complete = interpretation.complete
//     let assertion = interpretation.current.assertions.MinimumElement
//     let mutable incomplete = interpretation.incomplete.MinimumElement

//     let alt =
//         { assertions = Set.remove assertion alt.assertions
//           roles = alt.roles
//           attributes = alt.attributes
//           individuals = alt.individuals }

//     List.iter
//         (fun value ->
//             match value with
//             | Alternative.Complete newComlete -> complete <- Set.add newComlete complete
//             | Alternative.Incomplete newIncomplete -> incomplete <- List.append [ newIncomplete ] incomplete)
//         (interpretAssertion assertion alt)

//     interpretABox incomplete complete

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
        interpretABox (newInterpretation aBox')

let rec isConsistent (interpretation: Interpretation) : Result<bool, LigatureError> =
    match interpretation with
    | None -> Ok false
    | Some interpretation ->
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
            interpretation.individuals

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
