// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Interpreter

open Ligature.Model

type PotentialModel =
    { assertions: Assertions
      skip: Assertions
      isA: Map<Term, Set<Term>>
      isNot: Map<Term, Set<Term>>
      roles: Set<Term * Term * Term>
      attributes: Set<Term * Term * Literal> }

let addInstance (individual: Term) (concept: Term) (map: Map<Term, Set<Term>>) =
    match map.TryFind individual with
    | Some concepts -> Map.add individual (Set.add concept concepts) map
    | None -> Map.add individual (Set.ofList [ concept ]) map

let modelToAssertions (potentialModel: PotentialModel) : Assertions =
    if not potentialModel.assertions.IsEmpty then
        failwith "Invalid call to modelToAssertions."

    Set.unionMany
        [ Set.map (fun (i, a, l) -> Assertion.Triple(i, a, Value.Literal l)) potentialModel.attributes

          Set.map (fun (i, r, t) -> Assertion.Triple(i, r, Value.Term t)) potentialModel.roles

          Map.fold
              (fun state key value ->
                  Set.fold
                      (fun state value -> Set.add (Assertion.Instance(key, ConceptExpr.AtomicConcept value)) state)
                      state
                      value)
              Set.empty
              potentialModel.isA

          Map.fold
              (fun state key value ->
                  Set.fold
                      (fun state value ->
                          Set.add (Assertion.Instance(key, ConceptExpr.Not(ConceptExpr.AtomicConcept value))) state)
                      state
                      value)
              Set.empty
              potentialModel.isNot ]

let newModel (assertions: Assertions) : PotentialModel =
    { assertions = assertions
      skip = Set.empty
      isA = Map.empty
      isNot = Map.empty
      roles = Set.empty
      attributes = Set.empty }

let tBoxToMap (tBox: Definitions) : Option<Map<Term, ConceptExpr>> =
    List.fold
        (fun state value ->
            match value with
            | ConceptExpr.Equivalent(ConceptExpr.AtomicConcept a, c) ->
                if state.Value.ContainsKey a then
                    None
                else
                    Some(Map.add a c state.Value)
            | ConceptExpr.Implies(ConceptExpr.AtomicConcept a, c) ->
                if state.Value.ContainsKey a then
                    None
                else
                    Some(Map.add a c state.Value)
            | _ -> None)
        (Some Map.empty)
        tBox

let isDefinitorial (definitions: Definitions) : bool =
    let mutable checkedConcepts = Set.empty

    let rec hasCycle (definitionsMap: Map<Term, ConceptExpr>) (concept: Term) (definition: ConceptExpr) : bool =
        match definition with
        | ConceptExpr.AtomicConcept a ->
            if concept = a then
                true
            else if checkedConcepts.Contains a then
                false
            else
                match Map.tryFind a definitionsMap with
                | None ->
                    checkedConcepts <- Set.add a checkedConcepts
                    false
                | Some c -> hasCycle definitionsMap concept c
        | ConceptExpr.Not(ConceptExpr.AtomicConcept a) ->
            if concept = a then
                true
            else if checkedConcepts.Contains a then
                false
            else
                match Map.tryFind a definitionsMap with
                | None ->
                    checkedConcepts <- Set.add a checkedConcepts
                    false
                | Some c -> hasCycle definitionsMap concept c
        | ConceptExpr.And conj ->
            List.forall (fun value -> not (hasCycle definitionsMap concept value)) conj
            |> not
        | ConceptExpr.Not(ConceptExpr.And conj) ->
            List.forall (fun value -> not (hasCycle definitionsMap concept value)) conj
            |> not

        | ConceptExpr.Or disj ->
            List.forall (fun value -> not (hasCycle definitionsMap concept value)) disj
            |> not
        | ConceptExpr.Not(ConceptExpr.Or disj) ->
            List.forall (fun value -> not (hasCycle definitionsMap concept value)) disj
            |> not

        | ConceptExpr.Top -> false
        | ConceptExpr.Bottom -> false
        | ConceptExpr.Not(ConceptExpr.Top) -> false
        | ConceptExpr.Not(ConceptExpr.Bottom) -> false
        | ConceptExpr.Exists(roleName, c) -> hasCycle definitionsMap concept c
        | ConceptExpr.Not(ConceptExpr.Exists(roleName, c)) -> hasCycle definitionsMap concept c
        | ConceptExpr.All(roleName, c) -> hasCycle definitionsMap concept c
        | ConceptExpr.Not(ConceptExpr.All(roleName, c)) -> hasCycle definitionsMap concept c

    match tBoxToMap definitions with
    | Some map ->
        Map.fold
            (fun state key value ->
                if state then
                    if hasCycle map key value then false else true
                else
                    state)
            true
            map
    | None -> false

let rec unfoldSingleExpression (definitions: Map<Term, ConceptExpr>) (expr: ConceptExpr) : ConceptExpr =
    match expr with
    | ConceptExpr.AtomicConcept ac ->
        match definitions.TryFind ac with
        | Some c -> c
        | None -> expr
    | ConceptExpr.And conj ->
        let conj = List.map (fun value -> unfoldSingleExpression definitions value) conj
        ConceptExpr.And conj
    | ConceptExpr.Or disj ->
        let disj = List.map (fun value -> unfoldSingleExpression definitions value) disj
        ConceptExpr.Or disj
    | ConceptExpr.Top -> ConceptExpr.Top
    | ConceptExpr.Bottom -> ConceptExpr.Bottom
    | ConceptExpr.Exists(roleName, c) ->
        let c = unfoldSingleExpression definitions c
        ConceptExpr.Exists(roleName, c)
    | ConceptExpr.All(roleName, c) ->
        let c = unfoldSingleExpression definitions c
        ConceptExpr.All(roleName, c)
    | ConceptExpr.Not c ->
        let c = unfoldSingleExpression definitions c
        ConceptExpr.Not c
    | ConceptExpr.Implies(_, _) -> failwith "Not Implemented"
    | ConceptExpr.Equivalent(_, _) -> failwith "Not Implemented"

let rec unfoldTBox (definitions: Map<Term, ConceptExpr>) (aBox: Assertions) : Assertions =
    let res =
        Set.map
            (fun assertion ->
                match assertion with
                | Assertion.Instance(i, c) -> Assertion.Instance(i, unfoldSingleExpression definitions c)
                | t -> t)
            aBox

    if res = aBox then res else unfoldTBox definitions res

let unfold tBox aBox : Result<Assertions, LigatureError> =
    if isDefinitorial tBox then
        match tBoxToMap tBox with
        | Some value -> Ok(unfoldTBox value aBox)
        | None -> failwith "TODO"
    else
        failwith "TODO"

let handleTBox (tBox: Definitions) (aBox: Assertions) : Result<Assertions, LigatureError> =
    if tBox.IsEmpty then
        Ok aBox
    else if isDefinitorial tBox then
        match tBoxToMap tBox with
        | Some map -> Ok(unfoldTBox map aBox)
        | None -> error "Only definitorial TBoxes are supported currently." None
    else
        error "Only definitorial TBoxes are supported currently." None

//     let setAlternatives (alternatives: List<Assertions>) =
//         match alternatives with
//         | [] -> ()
//         | [ single ] -> current.Value.assertions <- single
//         | head :: tail ->
//             current.Value.assertions <- head

//             List.iter
//                 (fun value ->
//                     incomplete <-
//                         { assertions = value
//                           roles = current.Value.roles
//                           isA = current.Value.isA
//                           isNot = current.Value.isNot
//                           attributes = current.Value.attributes }
//                         :: incomplete)
//                 tail

//     // let addInstance (individual: Term) (isA: Set<Term>) (isNot: Set<Term>) =
//     //     failwith "TODO"
//     //     // let individuals =
//     //     //     match current.Value.isA.TryFind individual with
//     //     //     | Some { isA = isA'; isNot = isNot' } ->
//     //     //         Map.add
//     //     //             individual
//     //     //             { isA = Set.union isA' isA
//     //     //               isNot = Set.union isNot' isNot }
//     //     //             current.Value.individuals
//     //     //     | None -> Map.add individual { isA = isA; isNot = isNot } current.Value.individuals

//     //     // current.Value.individuals <- individuals

//     let isConsistent (model: PotentialModel) : bool =
//         failwith "TODO"
//         // Map.fold
//         //     (fun state _ { isA = isA; isNot = isNot } ->
//         //         match state with
//         //         | true -> if (Set.intersect isA isNot).IsEmpty then true else false
//         //         | e -> e)
//         //     true
//         //     model.individuals

//     let complete () =
//         if isConsistent current.Value then
//             _model <-
//                 Some
//                     { isA = current.Value.isA
//                       isNot = current.Value.isNot
//                       attributes = current.Value.attributes
//                       roles = current.Value.roles }

//             current <- None
//         else
//             current <- None

let interpretNextAssertion (state: PotentialModel) : PotentialModel * PotentialModel list =
    if state.assertions.IsEmpty then
        if state.skip.IsEmpty then
            state, []
        else
            { state with
                assertions = state.skip
                skip = Set.empty },
            []
    else
        let assertion = state.assertions.MinimumElement
        let assertions = Set.remove assertion state.assertions

        match assertion with
        | Assertion.Instance(_, ConceptExpr.Equivalent _) -> failwith "Unexpected value."
        | Assertion.Instance(_, ConceptExpr.Implies _) -> failwith "Unexpected value."
        | Assertion.Instance(_, ConceptExpr.Top) -> { state with assertions = assertions }, []
        | Assertion.Instance(_, ConceptExpr.Bottom) -> failwith "Unexpected value"
        | Assertion.Instance(individual, ConceptExpr.AtomicConcept concept) ->
            { state with
                assertions = Set.remove assertion state.assertions
                isA = addInstance individual concept state.isA },
            []
        | Assertion.Instance(individual, ConceptExpr.Not(ConceptExpr.AtomicConcept concept)) ->
            { state with
                assertions = Set.remove assertion state.assertions
                isNot = addInstance individual concept state.isNot },
            []
        | Assertion.Instance(individual, ConceptExpr.And group) ->
            let mutable assertions = Set.remove assertion state.assertions
            List.iter (fun expr -> assertions <- Set.add (Assertion.Instance(individual, expr)) assertions) group

            { state with
                assertions = Set.remove assertion state.assertions },
            []
        | Assertion.Instance(individual, ConceptExpr.Not(ConceptExpr.And group)) ->
            let mutable assertions = Set.remove assertion state.assertions
            let negGroup = List.map (fun value -> ConceptExpr.Not value) group
            assertions <- Set.add (Assertion.Instance(individual, ConceptExpr.Or negGroup)) assertions

            { state with
                assertions = Set.remove assertion state.assertions },
            []

        | Assertion.Instance(individual, ConceptExpr.Or group) -> failwith "TODO"
        // let mutable assertions = Set.remove assertion current.Value.assertions

        // let alternatives: List<Assertions> =
        //     List.fold
        //         (fun state expr -> Set.add (Assertion.Instance(individual, expr)) assertions :: state)
        //         []
        //         group

        // setAlternatives alternatives

        // if current.Value.assertions.IsEmpty then
        //     complete ()
        | Assertion.Instance(individual, ConceptExpr.All(role, concept)) ->
            if
                not (
                    Set.exists
                        (fun value ->
                            match value with
                            | Assertion.Triple _ -> true
                            | _ -> false)
                        state.assertions
                )
            then
                let assertions = Set.remove assertion state.assertions

                //TODO find all instances of the given role and mark all fillers as being `concept`
                // let assertions =
                //     Set.fold
                //         (fun state assertion ->
                //             match assertion with
                //             | Assertion.Triple(i, r, Value.Term f) when r = role && i = individual ->
                //                 Set.add (Assertion.Instance(f, concept)) state
                //             | _ -> state)
                //         assertions
                //         assertions


                let assertions =
                    Set.fold
                        (fun state value ->
                            match value with
                            | i, r, f when r = role && i = individual -> Set.add (Assertion.Instance(f, concept)) state
                            | _ -> state)
                        assertions
                        state.roles

                { state with assertions = assertions }, []
            //TODO handle inconsistent ConceptExprs
            // if assertions.IsEmpty then
            //     complete ()
            else
                failwith "TODO"
                state, [] //wait to process
        | Assertion.Instance(individual, ConceptExpr.Not(ConceptExpr.All(roleName, concept))) ->
            let assertions =
                Set.remove assertion state.assertions
                |> Set.add (Assertion.Instance(individual, ConceptExpr.Exists(roleName, ConceptExpr.Not concept)))

            { state with assertions = assertions }, []
        | Assertion.Instance(individual, ConceptExpr.Exists(roleName, concept)) ->
            if
                not (
                    Set.exists
                        (fun value ->
                            match value with
                            | Assertion.Triple _ -> true
                            | _ -> false)
                        state.assertions
                )
            then
                //TODO handle inconsistent ConceptExprs
                //addInstance individual Set.empty Set.empty
                let assertions = Set.remove assertion state.assertions

                let r = new System.Random()
                let newIndividual = Term $"new-{r.Next()}"

                let assertions =
                    Set.add (Assertion.Triple(individual, roleName, Value.Term newIndividual)) assertions

                let assertions = Set.add (Assertion.Instance(newIndividual, concept)) assertions

                { state with assertions = assertions }, [] //TODO this isn't complete
            else
                failwith "TODO" //add assertion to skip
        | Assertion.Instance(individual, ConceptExpr.Not(ConceptExpr.Exists(roleName, concept))) ->
            let assertions =
                Set.remove assertion state.assertions
                |> Set.add (Assertion.Instance(individual, ConceptExpr.All(roleName, ConceptExpr.Not concept)))

            { state with assertions = assertions }, []

        | Assertion.Instance(individual, ConceptExpr.Not(ConceptExpr.Not(concept))) ->
            let assertions =
                Set.remove assertion state.assertions
                |> Set.add (Assertion.Instance(individual, concept))

            { state with assertions = assertions }, []


        | Assertion.Instance(individual, ConceptExpr.Not(concept)) ->
            let assertions =
                Set.remove assertion state.assertions
                |> Set.add (Assertion.Instance(individual, ConceptExpr.Not(concept)))

            { state with assertions = assertions }, []


        // match concept with
        // | ConceptExpr.Or group -> failwith "TODO"
        // // let mutable assertions = Set.remove assertion current.Value.assertions
        // // let negGroup = List.map (fun value -> ConceptExpr.Not value) group
        // // assertions <- Set.add (Assertion.Instance(individual, ConceptExpr.And negGroup)) assertions
        // // setAssertions assertions
        // | ConceptExpr.Top -> failwith "TODO" //setAssertions (Set.remove assertion current.Value.assertions)
        // | ConceptExpr.Bottom -> failwith "TODO" //setAssertions (Set.remove assertion current.Value.assertions)
        // | ConceptExpr.Not concept ->
        | Assertion.Triple(i, r, Value.Term t) ->
            { state with
                assertions = Set.remove assertion state.assertions
                roles = Set.add (i, r, t) state.roles },
            []
        | Assertion.Triple(i, a, Value.Literal l) ->
            { state with
                assertions = Set.remove assertion state.assertions
                attributes = Set.add (i, a, l) state.attributes },
            []

let containsClash (model: PotentialModel) : bool =
    Map.fold
        (fun state individual concepts ->
            if state = false then
                match model.isNot.TryFind individual with
                | Some notConcepts -> not (Set.intersect concepts notConcepts).IsEmpty
                | None -> false
            else
                state)
        false
        model.isA

let findModel definitions assertions : Result<Assertions option, LigatureError> =
    let mutable result = None

    let mutable currentModel: PotentialModel option =
        match handleTBox definitions assertions with
        | Ok assertions -> Some(newModel assertions)
        | _ -> None

    let mutable additionalModels: PotentialModel list = []

    while result.IsNone && currentModel.IsSome do
        match currentModel with
        | Some model ->
            if model.assertions.IsEmpty then
                if model.skip.IsEmpty then
                    if containsClash model then
                        match additionalModels with
                        | head :: tail ->
                            currentModel <- Some head
                            additionalModels <- tail
                        | [] -> currentModel <- None
                    else
                        result <- Some model
                else
                    currentModel <-
                        Some
                            { model with
                                assertions = model.skip
                                skip = Set.empty }
            else
                let nextModel, newPotentialModels = interpretNextAssertion model
                currentModel <- Some nextModel
                additionalModels <- List.append additionalModels newPotentialModels
        | None -> failwith "TODO"

    match result with
    | Some result -> Ok(Some(modelToAssertions result))
    | None -> Ok None

let nnf (definitions: Definitions) : Result<ConceptExpr, LigatureError> =
    let rec nnfConcept (conceptExpr: ConceptExpr) : ConceptExpr =
        match conceptExpr with
        | ConceptExpr.AtomicConcept c -> ConceptExpr.AtomicConcept c
        | ConceptExpr.And conj -> List.map (fun value -> nnfConcept value) conj |> ConceptExpr.And
        | ConceptExpr.Or disj -> List.map (fun value -> nnfConcept value) disj |> ConceptExpr.Or
        | ConceptExpr.Top -> ConceptExpr.Top
        | ConceptExpr.Bottom -> ConceptExpr.Bottom
        | ConceptExpr.All(r, c) -> ConceptExpr.All(r, nnfConcept c)
        | ConceptExpr.Exists(r, c) -> ConceptExpr.Exists(r, nnfConcept c)
        | ConceptExpr.Not(ConceptExpr.Not not') -> nnfConcept not'
        | ConceptExpr.Not(ConceptExpr.And conj) ->
            List.map (fun value -> ConceptExpr.Not(nnfConcept value)) conj |> ConceptExpr.Or
        | ConceptExpr.Not(ConceptExpr.Or disj) ->
            List.map (fun value -> ConceptExpr.Not(nnfConcept value)) disj
            |> ConceptExpr.And
        | ConceptExpr.Not c -> ConceptExpr.Not(nnfConcept c)
        | ConceptExpr.Implies(lhs, rhs) -> ConceptExpr.Or [ rhs; ConceptExpr.Not lhs ]
        | ConceptExpr.Equivalent(lhs, rhs) ->
            ConceptExpr.And
                [ ConceptExpr.Or [ rhs; ConceptExpr.Not lhs ]
                  ConceptExpr.Or [ lhs; ConceptExpr.Not rhs ] ]

    and nnf (definitions': Definitions) : ConceptExpr =
        if not definitions'.IsEmpty then
            List.map (fun value -> nnfConcept value) definitions' |> ConceptExpr.And
        else
            ConceptExpr.Top

    Ok(nnf definitions)

let isConsistent definitions assertions : Result<bool, LigatureError> =
    match findModel definitions assertions with
    | Ok(Some _) -> Ok true
    | Ok None -> Ok false
    | Error err -> Error err
