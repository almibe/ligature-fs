// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Interpreter

open Ligature.Model

type ConceptValues = { isA: Set<Term>; isNot: Set<Term> }

type Model =
    { individuals: Map<Term, ConceptValues>
      roles: Map<Term * Term, Set<Term>>
      attributes: Map<Term * Literal, Set<Term>> }

type IncompleteModel =
    { assertions: Assertions
      individuals: Map<Term, ConceptValues>
      roles: Map<Term * Term, Set<Term>>
      attributes: Map<Term * Literal, Set<Term>> }

let newModel assertions =
    { assertions = assertions
      individuals = Map.empty
      roles = Map.empty
      attributes = Map.empty }

type Interpretation(_definitions, _assertions) =
    let mutable current: IncompleteModel option = Some(newModel _assertions)
    let mutable incomplete: List<IncompleteModel> = []
    let mutable clash: Set<Model> = Set.empty
    let mutable _model: Model option = None

    let setAssertions (assertions: Assertions) =
        current <-
            Some
                { assertions = assertions
                  roles = current.Value.roles
                  individuals = current.Value.individuals
                  attributes = current.Value.attributes }

    let setAlternatives (alternatives: List<Assertions>) =
        match alternatives with
        | [] -> ()
        | [ single ] ->
            current <-
                Some
                    { assertions = single
                      roles = current.Value.roles
                      individuals = current.Value.individuals
                      attributes = current.Value.attributes }
        | head :: tail ->
            current <-
                Some
                    { assertions = head
                      roles = current.Value.roles
                      individuals = current.Value.individuals
                      attributes = current.Value.attributes }

            List.iter
                (fun value ->
                    incomplete <-
                        { assertions = value
                          roles = current.Value.roles
                          individuals = current.Value.individuals
                          attributes = current.Value.attributes }
                        :: incomplete)
                tail

    let addInstance (individual: Term) (isA: Set<Term>) (isNot: Set<Term>) =
        let individuals =
            match current.Value.individuals.TryFind individual with
            | Some { isA = isA'; isNot = isNot' } ->
                Map.add
                    individual
                    { isA = Set.union isA' isA
                      isNot = Set.union isNot' isNot }
                    current.Value.individuals
            | None -> Map.add individual { isA = isA; isNot = isNot } current.Value.individuals

        current <-
            Some
                { individuals = individuals
                  roles = current.Value.roles
                  attributes = current.Value.attributes
                  assertions = current.Value.assertions }

    let succeed () =
        _model <-
            Some
                { individuals = current.Value.individuals
                  attributes = current.Value.attributes
                  roles = current.Value.roles }

        current <- None


    let interpretNextAssertion () =
        let assertion = current.Value.assertions.MinimumElement

        match assertion with
        | Assertion.IsA(individual, ConceptExpr.AtomicConcept concept) ->
            let assertions = Set.remove assertion current.Value.assertions
            setAssertions assertions

            if assertions.IsEmpty then
                addInstance individual (Set.ofList [ concept ]) Set.empty
                succeed ()
            else
                addInstance individual (Set.ofList [ concept ]) Set.empty

        | Assertion.IsA(individual, ConceptExpr.And group) ->
            let mutable assertions = Set.remove assertion current.Value.assertions
            List.iter (fun expr -> assertions <- Set.add (Assertion.IsA(individual, expr)) assertions) group
            setAssertions assertions

            if assertions.IsEmpty then
                succeed ()
        | Assertion.IsA(individual, ConceptExpr.Or group) ->
            let mutable assertions = Set.remove assertion current.Value.assertions

            let alternatives: List<Assertions> =
                List.fold (fun state expr -> Set.add (Assertion.IsA(individual, expr)) assertions :: state) [] group

            setAlternatives alternatives

            if assertions.IsEmpty then
                succeed ()
        | Assertion.IsA(individual, ConceptExpr.All(role, concept)) ->

            failwith "TODO"
        | Assertion.IsA(individual, ConceptExpr.Exists(role, concept)) ->

            failwith "TODO"
        | Assertion.IsA(individual, ConceptExpr.Not(concept)) ->
            match concept with
            | ConceptExpr.AtomicConcept concept ->
                let assertions = Set.remove assertion current.Value.assertions
                setAssertions assertions

                if assertions.IsEmpty then
                    addInstance individual Set.empty (Set.ofList [ concept ])
                    succeed ()
                else
                    addInstance individual Set.empty (Set.ofList [ concept ])
            | ConceptExpr.And(_) -> failwith "Not Implemented"
            | ConceptExpr.Or(_) -> failwith "Not Implemented"
            | ConceptExpr.Top -> failwith "Not Implemented"
            | ConceptExpr.Bottom -> failwith "Not Implemented"
            | ConceptExpr.Exists(_, _) -> failwith "Not Implemented"
            | ConceptExpr.All(_, _) -> failwith "Not Implemented"
            | ConceptExpr.Not concept ->
                let assertions = Set.remove assertion current.Value.assertions
                let assertions = Set.add (Assertion.IsA(individual, concept)) assertions
                setAssertions assertions
        | Assertion.Triple(e, a, Value.Term v) -> failwith "TODO"
        | Assertion.Triple(e, a, Value.Literal l) -> failwith "TODO"

    let rec processCurrent () =
        if current.IsNone || current.Value.assertions.IsEmpty then
            match incomplete with
            | [] -> current <- None
            | head :: tail ->
                current <- Some head
                incomplete <- tail
        else
            interpretNextAssertion ()
            processCurrent ()
    // // if interpretation.incomplete.IsEmpty then
    // //     toInterpretation interpretation
    // // else
    // //     let mutable complete = interpretation.complete
    // //     let assertion = interpretation.current.assertions.MinimumElement
    // //     let mutable incomplete = interpretation.incomplete.MinimumElement

    // //     let alt =
    // //         { assertions = Set.remove assertion alt.assertions
    // //           roles = alt.roles
    // //           attributes = alt.attributes
    // //           individuals = alt.individuals }

    // //     List.iter
    // //         (fun value ->
    // //             match value with
    // //             | Alternative.Complete newComlete -> complete <- Set.add newComlete complete
    // //             | Alternative.Incomplete newIncomplete -> incomplete <- List.append [ newIncomplete ] incomplete)
    // //         (interpretAssertion assertion alt)

    // //     interpretABox incomplete complete

    do
        processCurrent ()

        while _model = None && not incomplete.IsEmpty do
            incomplete <- incomplete.Tail
            current <- Some incomplete.Head
            processCurrent ()

    member _.model = _model

//let addRole (interpretation: InterpretationStep) e a v = failwith "TODO"
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



// let interpret (tBox: Definitions) (aBox: Assertions) : Interpretation =
//     let aBox' =
//         if tBox.IsEmpty then
//             aBox
//         else
//             Set.fold
//                 (fun state definition ->
//                     match definition with
//                     | Definition.Implies(a, c) ->
//                         match c with
//                         | ConceptExpr.AtomicConcept c ->
//                             Set.fold
//                                 (fun state value ->
//                                     match value with
//                                     | Assertion.IsA(ind, ConceptExpr.AtomicConcept concept) when concept = a ->
//                                         Set.add (Assertion.IsA(ind, ConceptExpr.AtomicConcept c)) state
//                                     | _ -> state)
//                                 state
//                                 aBox
//                         | ConceptExpr.And conj -> failwith "TODO"
//                         | _ -> failwith "TODO"
//                     | Definition.Define(a, c) -> failwith "TODO")
//                 aBox
//                 tBox

//     if aBox <> aBox' then
//         interpret tBox aBox'
//     else
//         interpretABox (newInterpretation aBox')

let isConsistent definitions assertions : Result<bool, LigatureError> =
    let interpretation = new Interpretation(definitions, assertions)
    // printfn $"{interpretation.model}"
    match interpretation.model with
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
