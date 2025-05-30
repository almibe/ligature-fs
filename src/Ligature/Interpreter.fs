// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Interpreter

open Ligature.Model

type PotentialModel =
    { toProcess: ABox
      skip: ABox
      isA: Map<Term, Set<Term>>
      isNot: Map<Term, Set<Term>>
      isEq: Map<Term, Set<Term>>
      isNotEq: Map<Term, Set<Term>>
      roles: Set<Term * Term * Term>
      attributes: Set<Term * Term * Literal> }

let addInstance (individual: Term) (concept: Term) (map: Map<Term, Set<Term>>) =
    match map.TryFind individual with
    | Some concepts -> Map.add individual (Set.add concept concepts) map
    | None -> Map.add individual (Set.ofList [ concept ]) map

let modelToAssertions (potentialModel: PotentialModel) : ABox =
    if not potentialModel.toProcess.IsEmpty then
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

let newModel (aBox: ABox) : PotentialModel =
    { toProcess = aBox
      skip = Set.empty
      isA = Map.empty
      isNot = Map.empty
      isEq = Map.empty
      isNotEq = Map.empty
      roles = Set.empty
      attributes = Set.empty }

let tBoxToMap (tBox: TBox) : Option<Map<Term, ConceptExpr>> =
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

let isDefinitorial (tBox: TBox) : bool =
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
        | ConceptExpr.Exactly(_, c, _) -> hasCycle definitionsMap concept c
        | ConceptExpr.AtLeast(_, c, _) -> hasCycle definitionsMap concept c
        | ConceptExpr.AtMost(_, c, _) -> hasCycle definitionsMap concept c

    match tBoxToMap tBox with
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
    | ConceptExpr.Exactly(roleName, c, number) ->
        let c = unfoldSingleExpression definitions c
        ConceptExpr.Exactly(roleName, c, number)
    | ConceptExpr.AtLeast(roleName, c, number) ->
        let c = unfoldSingleExpression definitions c
        ConceptExpr.AtLeast(roleName, c, number)
    | ConceptExpr.AtMost(roleName, c, number) ->
        let c = unfoldSingleExpression definitions c
        ConceptExpr.AtMost(roleName, c, number)
    | ConceptExpr.Implies(_, _) -> failwith "Not Implemented"
    | ConceptExpr.Equivalent(_, _) -> failwith "Not Implemented"

let rec unfoldTBox (definitions: Map<Term, ConceptExpr>) (aBox: ABox) : ABox =
    let res =
        Set.map
            (fun assertion ->
                match assertion with
                | Assertion.Instance(i, c) -> Assertion.Instance(i, unfoldSingleExpression definitions c)
                | t -> t)
            aBox

    if res = aBox then res else unfoldTBox definitions res

let unfold tBox aBox : Result<ABox, LigatureError> =
    if isDefinitorial tBox then
        match tBoxToMap tBox with
        | Some value -> Ok(unfoldTBox value aBox)
        | None -> failwith "TODO"
    else
        failwith "TODO"

let handleTBox (tBox: TBox) (aBox: ABox) : Result<ABox, LigatureError> =
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
    if state.toProcess.IsEmpty then
        if state.skip.IsEmpty then
            state, []
        else
            { state with
                toProcess = state.skip
                skip = Set.empty },
            []
    else
        let assertion = state.toProcess.MinimumElement
        let assertions = Set.remove assertion state.toProcess

        match assertion with
        | Assertion.Instance(_, ConceptExpr.Equivalent _) -> failwith "Unexpected value."
        | Assertion.Instance(_, ConceptExpr.Implies _) -> failwith "Unexpected value."
        | Assertion.Instance(_, ConceptExpr.Top) -> { state with toProcess = assertions }, []
        | Assertion.Instance(_, ConceptExpr.Bottom) -> failwith "Unexpected value"
        | Assertion.Instance(individual, ConceptExpr.AtomicConcept concept) ->
            { state with
                toProcess = Set.remove assertion state.toProcess
                isA = addInstance individual concept state.isA },
            []
        | Assertion.Instance(individual, ConceptExpr.Not(ConceptExpr.AtomicConcept concept)) ->
            { state with
                toProcess = Set.remove assertion state.toProcess
                isNot = addInstance individual concept state.isNot },
            []
        | Assertion.Instance(individual, ConceptExpr.And group) ->
            let mutable assertions = Set.remove assertion state.toProcess
            List.iter (fun expr -> assertions <- Set.add (Assertion.Instance(individual, expr)) assertions) group

            { state with toProcess = assertions }, []
        | Assertion.Instance(individual, ConceptExpr.Not(ConceptExpr.And group)) ->
            let mutable assertions = Set.remove assertion state.toProcess
            let negGroup = List.map (fun value -> ConceptExpr.Not value) group
            assertions <- Set.add (Assertion.Instance(individual, ConceptExpr.Or negGroup)) assertions

            { state with
                toProcess = Set.remove assertion state.toProcess },
            []

        | Assertion.Instance(individual, ConceptExpr.Or group) ->
            let mutable assertions = Set.remove assertion state.toProcess

            let models: List<PotentialModel> =
                List.fold
                    (fun models expr -> 
                        let asserts = Set.add (Assertion.Instance(individual, expr)) assertions
                        {state with toProcess = asserts} :: models)
                    []
                    group

            models.Head, models.Tail
        | Assertion.Instance(individual, ConceptExpr.Not(ConceptExpr.Or group)) ->
            let assertions = Set.remove assertion state.toProcess
            let negGroup = List.map (fun value -> ConceptExpr.Not value) group
            let assertions = Set.add (Assertion.Instance(individual, ConceptExpr.And negGroup)) assertions
            {state with toProcess = assertions}, []
        | Assertion.Instance(individual, ConceptExpr.All(role, concept)) ->
            if
                not (
                    Set.exists
                        (fun value ->
                            match value with
                            | Assertion.Triple _ -> true
                            | _ -> false)
                        state.toProcess
                )
            then
                let assertions = Set.remove assertion state.toProcess

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

                { state with toProcess = assertions }, []
            //TODO handle inconsistent ConceptExprs
            // if assertions.IsEmpty then
            //     complete ()
            else
                failwith "TODO"
                state, [] //wait to process
        | Assertion.Instance(individual, ConceptExpr.Not(ConceptExpr.All(roleName, concept))) ->
            let assertions =
                Set.remove assertion state.toProcess
                |> Set.add (Assertion.Instance(individual, ConceptExpr.Exists(roleName, ConceptExpr.Not concept)))

            { state with toProcess = assertions }, []
        | Assertion.Instance(individual, ConceptExpr.Exists(roleName, concept)) ->
            if
                not (
                    Set.exists
                        (fun value ->
                            match value with
                            | Assertion.Triple _ -> true
                            | _ -> false)
                        state.toProcess
                )
            then
                //TODO handle inconsistent ConceptExprs
                //addInstance individual Set.empty Set.empty
                let assertions = Set.remove assertion state.toProcess

                let r = new System.Random()
                let newIndividual = Term $"new-{r.Next()}"

                let assertions =
                    Set.add (Assertion.Triple(individual, roleName, Value.Term newIndividual)) assertions

                let assertions = Set.add (Assertion.Instance(newIndividual, concept)) assertions

                { state with toProcess = assertions }, [] //TODO this isn't complete
            else
                failwith "TODO" //add assertion to skip
        | Assertion.Instance(individual, ConceptExpr.Not(ConceptExpr.Exists(roleName, concept))) ->
            let assertions =
                Set.remove assertion state.toProcess
                |> Set.add (Assertion.Instance(individual, ConceptExpr.All(roleName, ConceptExpr.Not concept)))

            { state with toProcess = assertions }, []

        | Assertion.Instance(individual, ConceptExpr.Exactly(roleName, concept, number)) ->
            if
                not (
                    Set.exists
                        (fun value ->
                            match value with
                            | Assertion.Triple _ -> true
                            | _ -> false)
                        state.toProcess
                )
            then
                let matches =
                    Set.filter (fun (i, r, _) -> i = individual && r = roleName) state.roles

                let count = int64 matches.Count

                if count = number then
                    { state with toProcess = assertions }, []
                else
                    failwith "TODO"
            else
                //add to skip
                failwith "TODO"

        | Assertion.Instance(individual, ConceptExpr.AtLeast(roleName, concept, number)) -> failwith "TODO"

        | Assertion.Instance(individual, ConceptExpr.AtMost(roleName, concept, number)) -> failwith "TODO"


        | Assertion.Instance(individual, ConceptExpr.Not(ConceptExpr.Not(concept))) ->
            let assertions =
                Set.remove assertion state.toProcess
                |> Set.add (Assertion.Instance(individual, concept))

            { state with toProcess = assertions }, []


        | Assertion.Instance(individual, ConceptExpr.Not(concept)) ->
            let assertions =
                Set.remove assertion state.toProcess
                |> Set.add (Assertion.Instance(individual, ConceptExpr.Not(concept)))

            { state with toProcess = assertions }, []


        // match concept with
        // | ConceptExpr.Top -> failwith "TODO" //setAssertions (Set.remove assertion current.Value.assertions)
        // | ConceptExpr.Bottom -> failwith "TODO" //setAssertions (Set.remove assertion current.Value.assertions)
        // | ConceptExpr.Not concept ->
        | Assertion.Triple(i, r, Value.Term t) ->
            { state with
                toProcess = Set.remove assertion state.toProcess
                roles = Set.add (i, r, t) state.roles },
            []
        | Assertion.Triple(i, a, Value.Literal l) ->
            { state with
                toProcess = Set.remove assertion state.toProcess
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

let findModel definitions assertions : Result<ABox option, LigatureError> =
    let mutable result = None

    let mutable currentModel: PotentialModel option =
        match handleTBox definitions assertions with
        | Ok assertions -> Some(newModel assertions)
        | _ -> None

    let mutable additionalModels: PotentialModel list = []

    while result.IsNone && currentModel.IsSome do
        match currentModel with
        | Some model ->
            if model.toProcess.IsEmpty then
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
                                toProcess = model.skip
                                skip = Set.empty }
            else
                let nextModel, newPotentialModels = interpretNextAssertion model
                currentModel <- Some nextModel
                additionalModels <- List.append additionalModels newPotentialModels
        | None -> failwith "TODO"

    match result with
    | Some result -> Ok(Some(modelToAssertions result))
    | None -> Ok None

let nnf (definitions: TBox) : Result<ConceptExpr, LigatureError> =
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

    and nnf (definitions': TBox) : ConceptExpr =
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

let isInstance definitions assertions individual concept : Result<Term, LigatureError> =

    failwith "TODO"
