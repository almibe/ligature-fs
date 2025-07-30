// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Interpreter

open Ligature.Model
open Core

[<RequireQualifiedAccess>]
type SimpleConcept =
    | AtomicConcept of Term
    | Top
    | Bottom
    | Exists of Term * ConceptExpr
    | All of Term * ConceptExpr
    | Func of Term * ConceptExpr
    | Nominal of Element

type PotentialModel =
    { toProcess: Assertions
      skip: Assertions
      isA: Map<Element, Set<SimpleConcept>>
      isNot: Map<Element, Set<SimpleConcept>>
      triples: Set<Element * Term * Element> }

let addInstance (individual: Element) (concept: SimpleConcept) (map: Map<Element, Set<SimpleConcept>>) =
    match map.TryFind individual with
    | Some concepts -> Map.add individual (Set.add concept concepts) map
    | None -> Map.add individual (Set.ofList [ concept ]) map

let simpleToConceptExpr simple =
    Set.map
        (fun value ->
            match value with
            | SimpleConcept.AtomicConcept ac -> ConceptExpr.AtomicConcept ac
            | SimpleConcept.Top -> ConceptExpr.Top
            | SimpleConcept.Bottom -> ConceptExpr.Bottom
            | SimpleConcept.Exists(r, c) -> ConceptExpr.Exists(r, c)
            | SimpleConcept.All(r, c) -> ConceptExpr.All(r, c)
            | SimpleConcept.Func(r, c) -> ConceptExpr.Func(r, c)
            | SimpleConcept.Nominal e -> ConceptExpr.Nominal e)
        simple

let modelToAssertions (potentialModel: PotentialModel) : Assertions =
    if not potentialModel.toProcess.IsEmpty then
        failwith "Invalid call to modelToAssertions."

    Set.unionMany
        [ Set.map (fun (i, r, v) -> Assertion.Triple(i, r, v)) potentialModel.triples

          Map.fold
              (fun state key (value: Set<SimpleConcept>) ->
                  let value = simpleToConceptExpr value
                  Set.fold (fun state value -> Set.add (Assertion.Instance(key, value)) state) state value)
              Set.empty
              potentialModel.isA

          Map.fold
              (fun state key value ->
                  let value = simpleToConceptExpr value

                  Set.fold
                      (fun state value -> Set.add (Assertion.Instance(key, ConceptExpr.Not value)) state)
                      state
                      value)
              Set.empty
              potentialModel.isNot ]

let newModel (aBox: Assertions) : PotentialModel =
    { toProcess = aBox
      skip = Set.empty
      isA = Map.empty
      isNot = Map.empty
      triples = Set.empty }

let definitionsToMap (definitions: Definitions) : Option<Map<Term, ConceptExpr>> =
    let temp: Option<Map<Term, ConceptExpr list>> =
        Set.fold
            (fun state value ->
                match state, value with
                | Some state, Definition.Equivalent(ConceptExpr.AtomicConcept a, c) ->
                    match state.TryFind a with
                    | Some values -> Some(Map.add a (c :: values) state)
                    | None -> Some(Map.add a [ c ] state)
                | Some state, Definition.Implies(ConceptExpr.AtomicConcept a, c) ->
                    match state.TryFind a with
                    | Some values -> Some(Map.add a (c :: values) state)
                    | None -> Some(Map.add a [ c ] state)
                | _ -> None)
            (Some Map.empty)
            definitions

    Option.map
        (fun value ->
            Map.map
                (fun _ value ->
                    match value with
                    | [ single ] -> single
                    | concepts -> ConceptExpr.And concepts)
                value)
        temp

let isDefinitorial (tBox: Definitions) : bool =
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
        | ConceptExpr.Func _ -> false
        | ConceptExpr.Nominal _ -> false
    // | ConceptExpr.Exactly(_, c, _) -> hasCycle definitionsMap concept c
    // | ConceptExpr.AtLeast(_, c, _) -> hasCycle definitionsMap concept c
    // | ConceptExpr.AtMost(_, c, _) -> hasCycle definitionsMap concept c

    match definitionsToMap tBox with
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
    | ConceptExpr.Func(roleName, c) -> ConceptExpr.Func(roleName, c)
    | ConceptExpr.All(roleName, c) ->
        let c = unfoldSingleExpression definitions c
        ConceptExpr.All(roleName, c)
    | ConceptExpr.Not c ->
        let c = unfoldSingleExpression definitions c
        ConceptExpr.Not c
    | ConceptExpr.Nominal element -> ConceptExpr.Nominal element
// | ConceptExpr.Exactly(roleName, c, number) ->
//     let c = unfoldSingleExpression definitions c
//     ConceptExpr.Exactly(roleName, c, number)
// | ConceptExpr.AtLeast(roleName, c, number) ->
//     let c = unfoldSingleExpression definitions c
//     ConceptExpr.AtLeast(roleName, c, number)
// | ConceptExpr.AtMost(roleName, c, number) ->
//     let c = unfoldSingleExpression definitions c
//     ConceptExpr.AtMost(roleName, c, number)
// | ConceptExpr.Implies(_, _) -> failwith "Not Implemented"
// | ConceptExpr.Equivalent(_, _) -> failwith "Not Implemented"

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
        match definitionsToMap tBox with
        | Some value -> Ok(unfoldTBox value aBox)
        | None -> failwith "TODO"
    else
        failwith "TODO"

let handleTBox (tBox: Definitions) (aBox: Assertions) : Result<Assertions, LigatureError> =
    if tBox.IsEmpty then
        Ok aBox
    else if isDefinitorial tBox then
        match definitionsToMap tBox with
        | Some map -> Ok(unfoldTBox map aBox)
        | None -> error "Only definitorial TBoxes are supported currently." None
    else
        error "Only definitorial TBoxes are supported currently." None

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
        | Assertion.Instance(_, ConceptExpr.Top) -> { state with toProcess = assertions }, []
        | Assertion.Instance(_, ConceptExpr.Bottom) -> failwith "Unexpected value"
        | Assertion.Instance(individual, ConceptExpr.AtomicConcept concept) ->
            { state with
                toProcess = Set.remove assertion state.toProcess
                isA = addInstance individual (SimpleConcept.AtomicConcept concept) state.isA },
            []
        | Assertion.Instance(individual, ConceptExpr.Not(ConceptExpr.AtomicConcept concept)) ->
            { state with
                toProcess = Set.remove assertion state.toProcess
                isNot = addInstance individual (SimpleConcept.AtomicConcept concept) state.isNot },
            []
        | Assertion.Instance(individual, ConceptExpr.And group) ->
            let mutable assertions = Set.remove assertion state.toProcess
            List.iter (fun expr -> assertions <- Set.add (Assertion.Instance(individual, expr)) assertions) group

            { state with toProcess = assertions }, []
        | Assertion.Instance(individual, ConceptExpr.Not(ConceptExpr.And group)) ->
            let mutable assertions = Set.remove assertion state.toProcess
            let negGroup = List.map (fun value -> ConceptExpr.Not value) group
            assertions <- Set.add (Assertion.Instance(individual, ConceptExpr.Or negGroup)) assertions

            { state with toProcess = assertions }, []

        | Assertion.Instance(individual, ConceptExpr.Or group) ->
            let mutable assertions = Set.remove assertion state.toProcess

            let models: List<PotentialModel> =
                List.fold
                    (fun models expr ->
                        let asserts = Set.add (Assertion.Instance(individual, expr)) assertions
                        { state with toProcess = asserts } :: models)
                    []
                    group

            models.Head, models.Tail
        | Assertion.Instance(individual, ConceptExpr.Not(ConceptExpr.Or group)) ->
            let assertions = Set.remove assertion state.toProcess
            let negGroup = List.map (fun value -> ConceptExpr.Not value) group

            let assertions =
                Set.add (Assertion.Instance(individual, ConceptExpr.And negGroup)) assertions

            { state with toProcess = assertions }, []
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

                let assertions =
                    Set.fold
                        (fun state value ->
                            match value with
                            | i, r, f when r = role && i = individual -> Set.add (Assertion.Instance(f, concept)) state
                            | _ -> state)
                        assertions
                        state.triples

                { state with toProcess = assertions }, []
            //TODO handle inconsistent ConceptExprs
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

                let modelContainsRole =
                    Set.exists
                        (fun value ->
                            match value with
                            | i, r, v when individual = i && roleName = r -> true
                            | _ -> false)
                        state.triples

                if modelContainsRole then
                    { state with toProcess = assertions }, []
                else
                    let r = new System.Random()

                    let newInstance =
                        { value = Term $"new-{r.Next()}"
                          space = None
                          langTag = None }

                    let assertions =
                        Set.add (Assertion.Triple(individual, roleName, newInstance)) assertions

                    let assertions = Set.add (Assertion.Instance(newInstance, concept)) assertions

                    { state with toProcess = assertions }, []
            else
                failwith "TODO" //add assertion to skip
        | Assertion.Instance(individual, ConceptExpr.Not(ConceptExpr.Exists(roleName, concept))) ->
            let assertions =
                Set.remove assertion state.toProcess
                |> Set.add (Assertion.Instance(individual, ConceptExpr.All(roleName, ConceptExpr.Not concept)))

            { state with toProcess = assertions }, []
        | Assertion.Instance(individual, ConceptExpr.Func(roleName, c)) ->

            let assertions = Set.remove assertion state.toProcess

            { state with
                toProcess = assertions
                isA = addInstance individual (SimpleConcept.Func(roleName, c)) state.isA },
            []

        // let triplesMatches: Set<Element> =
        //     Set.filter
        //         (fun value ->
        //             match value with
        //             | i, r, _ when i = individual && r = roleName -> true
        //             | _ -> false)
        //         state.triples
        //     |> Set.map (fun (_, _, filler) -> filler)

        // let assertionsMatches: Set<Element> =
        //     Set.fold
        //         (fun state value ->
        //             match value with
        //             | Assertion.Triple(i, r, filler) when i = individual && r = roleName -> Set.add filler state
        //             | _ -> state)
        //         Set.empty
        //         (Set.union state.skip state.toProcess)

        // let allMatches = Set.union triplesMatches assertionsMatches

        // let assertions =
        //     Set.fold
        //         (fun state value ->
        //             Set.fold
        //                 (fun statei valuei ->
        //                     if value < valuei then
        //                         //Set.add (Assertion.Same(value, valuei)) statei
        //                         failwith "TODO"
        //                     else
        //                         statei)
        //                 state
        //                 allMatches)
        //         assertions
        //         allMatches

        // Some { state with toProcess = assertions }, []
        | Assertion.Instance(_, ConceptExpr.Not(ConceptExpr.Func _)) ->
            //no chance of clash here, just remove the assertion
            let assertions = Set.remove assertion state.toProcess
            { state with toProcess = assertions }, []
        // | Assertion.Instance(individual, ConceptExpr.Exactly(roleName, concept, number)) ->
        //     if
        //         not (
        //             Set.exists
        //                 (fun value ->
        //                     match value with
        //                     | Assertion.Triple _ -> true
        //                     | _ -> false)
        //                 state.toProcess
        //         )
        //     then
        //         let matches =
        //             Set.filter (fun (i, r, _) -> i = individual && r = roleName) state.roles

        //         let count = int64 matches.Count

        //         if count = number then
        //             { state with toProcess = assertions }, []
        //         else
        //             failwith "TODO"
        //     else
        //         //add to skip
        //         failwith "TODO"

        // | Assertion.Instance(individual, ConceptExpr.AtLeast(roleName, concept, number)) -> failwith "TODO"

        // | Assertion.Instance(individual, ConceptExpr.AtMost(roleName, concept, number)) -> failwith "TODO"


        | Assertion.Instance(individual, ConceptExpr.Not(ConceptExpr.Not concept)) ->
            let assertions =
                Set.remove assertion state.toProcess
                |> Set.add (Assertion.Instance(individual, concept))

            { state with toProcess = assertions }, []

        | Assertion.Triple(i, r, v) ->
            { state with
                toProcess = Set.remove assertion state.toProcess
                triples = Set.add (i, r, v) state.triples },
            []
        | Assertion.Instance(l, ConceptExpr.Nominal r) ->
            { state with
                toProcess = Set.remove assertion state.toProcess
                isA = addInstance l (SimpleConcept.Nominal r) state.isA },
            []
        | Assertion.Instance(l, ConceptExpr.Not(ConceptExpr.Nominal r)) -> failwith "TODO"
        // | Assertion.Different(l, r) ->
        // let clash =
        //     match state.same.TryFind l with
        //     | Some results -> results.Contains r
        //     | None -> false

        // if clash || l = r then
        //     None, []
        // else
        //     let different =
        //         match state.different.TryFind l with
        //         | Some results -> Map.add l (Set.add r results) state.different
        //         | None -> Map.add l (Set.ofList [ r ]) state.different

        //     let different =
        //         match different.TryFind r with
        //         | Some results -> Map.add r (Set.add l results) different
        //         | None -> Map.add r (Set.ofList [ l ]) different

        //     Some
        //         { state with
        //             toProcess = Set.remove assertion state.toProcess
        //             different = different },
        //     []
        | Assertion.Instance(i, ConceptExpr.Not ConceptExpr.Top) ->
            { state with
                toProcess = Set.remove assertion state.toProcess
                isA = addInstance i SimpleConcept.Top state.isA
                isNot = addInstance i SimpleConcept.Top state.isNot },
            []
        | Assertion.Instance(i, c) -> failwith $"Not Implemented: instance {i} {c}"

type ModelResult =
    { consistent: Assertions list
      clashed: Assertions list }

let containsClash (model: PotentialModel) : bool =
    Map.fold
        (fun state key value ->
            if not state then
                match model.isNot.TryFind key with
                | None -> false
                | Some isNot -> not (Set.intersect value isNot).IsEmpty
            else
                state)
        false
        model.isA

let tableauModels definitions assertions : Result<ModelResult, LigatureError> =
    let mutable currentModel: PotentialModel =
        match handleTBox definitions assertions with
        | Ok assertions -> newModel assertions
        | Error err -> failwith err.UserMessage

    let mutable additionalModels: PotentialModel list = []

    let mutable completedModelsClashFree = []
    let mutable completedModelsWithClash = []
    let mutable cont = true

    while cont do
        if currentModel.toProcess.IsEmpty then
            if currentModel.skip.IsEmpty then
                if containsClash currentModel then
                    completedModelsWithClash <- modelToAssertions currentModel :: completedModelsWithClash
                else
                    completedModelsClashFree <- modelToAssertions currentModel :: completedModelsClashFree

                match additionalModels with
                | head :: tail ->
                    currentModel <- head
                    additionalModels <- tail
                | [] -> cont <- false
            else
                currentModel <-
                    { currentModel with
                        toProcess = currentModel.skip
                        skip = Set.empty }
        else
            let nextModel, newPotentialModels = interpretNextAssertion currentModel

            currentModel <- nextModel
            additionalModels <- List.append additionalModels newPotentialModels

    Ok
        { consistent = completedModelsClashFree
          clashed = completedModelsWithClash }

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
        | ConceptExpr.Func(r, c) -> ConceptExpr.Func(r, c)
        | ConceptExpr.Not(ConceptExpr.Not not') -> nnfConcept not'
        | ConceptExpr.Not(ConceptExpr.And conj) ->
            List.map (fun value -> ConceptExpr.Not(nnfConcept value)) conj |> ConceptExpr.Or
        | ConceptExpr.Not(ConceptExpr.Or disj) ->
            List.map (fun value -> ConceptExpr.Not(nnfConcept value)) disj
            |> ConceptExpr.And
        | ConceptExpr.Not c -> ConceptExpr.Not(nnfConcept c)

    and nnf (definitions': Definitions) : ConceptExpr =
        if not definitions'.IsEmpty then
            Set.map
                (fun value ->
                    match value with
                    | Definition.Implies(lhs, rhs) -> ConceptExpr.Or [ rhs; ConceptExpr.Not lhs ]
                    | Definition.Equivalent(lhs, rhs) ->
                        ConceptExpr.And
                            [ ConceptExpr.Or [ rhs; ConceptExpr.Not lhs ]
                              ConceptExpr.Or [ lhs; ConceptExpr.Not rhs ] ])
                definitions'
            |> List.ofSeq
            |> ConceptExpr.And
        else
            ConceptExpr.Top

    Ok(nnf definitions)

let isConsistent definitions assertions : Result<bool, LigatureError> =
    match tableauModels definitions assertions with
    | Ok { consistent = [] } -> Ok false
    | Ok _ -> Ok true
    | Error err -> Error err

let isInstance
    (tBox: Definitions)
    (aBox: Assertions)
    (individual: Element)
    (concept: ConceptExpr)
    : Result<Term, LigatureError> =
    let modelResults =
        tableauModels tBox (Set.add (Assertion.Instance(individual, ConceptExpr.Not concept)) aBox)

    match modelResults with
    | Ok { consistent = [] } -> Ok(Term "true")
    | Ok { consistent = _; clashed = [] } -> Ok(Term "false")
    | Ok _ -> Ok(Term "unknown")
    | Error err -> error err.UserMessage None

let expandResult (aBox: Assertions) (individual: Element) (concept: ConceptExpr) : Assertions =

    Set.ofList [ Assertion.Instance(individual, concept) ]

let query (tBox: Definitions) (aBox: Assertions) (concept: ConceptExpr) : Element list =
    let individuals = individuals aBox

    // let res =
    List.filter
        (fun value ->
            match isInstance tBox aBox value concept with
            | Ok(Term "true") -> true
            | _ -> false)
        individuals
