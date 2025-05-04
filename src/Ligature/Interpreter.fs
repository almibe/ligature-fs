// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Interpreter

open Ligature.Model

type ConceptValues = { isA: Set<Term>; isNot: Set<Term> }

type Model =
    { individuals: Map<Term, ConceptValues>
      roles: Set<Term * Term * Term>
      attributes: Set<Term * Term * Literal> }

type IncompleteModel =
    { assertions: Assertions
      individuals: Map<Term, ConceptValues>
      roles: Set<Term * Term * Term>
      attributes: Set<Term * Term * Literal> }

let newModel assertions =
    { assertions = assertions
      individuals = Map.empty
      roles = Set.empty
      attributes = Set.empty }


let tBoxToMap (tBox: Definitions) : Option<Map<Term, ConceptExpr>> =
    Set.fold
        (fun state value ->
            match value with
            | Definition.Equivalent(ConceptExpr.AtomicConcept a, c) ->
                if state.Value.ContainsKey a then
                    None
                else
                    Some(Map.add a c state.Value)
            | Definition.Implies(ConceptExpr.AtomicConcept a, c) ->
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
        | ConceptExpr.And conj ->
            List.forall (fun value -> not (hasCycle definitionsMap concept value)) conj
            |> not
        | ConceptExpr.Or disj ->
            List.forall (fun value -> not (hasCycle definitionsMap concept value)) disj
            |> not
        | ConceptExpr.Top -> failwith "Not Implemented"
        | ConceptExpr.Bottom -> failwith "Not Implemented"
        | ConceptExpr.Exists(_, _) -> failwith "Not Implemented"
        | ConceptExpr.All(_, _) -> failwith "Not Implemented"
        | ConceptExpr.Not(_) -> failwith "Not Implemented"

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

let unfoldSingleExpression (definitions: Map<Term, ConceptExpr>) (expr: ConceptExpr) : ConceptExpr =
    match expr with
    | ConceptExpr.AtomicConcept c ->
        match definitions.TryFind c with
        | Some res -> res
        | None -> expr
    | ConceptExpr.And(_) -> failwith "Not Implemented"
    | ConceptExpr.Or(_) -> failwith "Not Implemented"
    | ConceptExpr.Top -> failwith "Not Implemented"
    | ConceptExpr.Bottom -> failwith "Not Implemented"
    | ConceptExpr.Exists(_, _) -> failwith "Not Implemented"
    | ConceptExpr.All(_, _) -> failwith "Not Implemented"
    | ConceptExpr.Not(_) -> failwith "Not Implemented"

let rec unfoldTBox (definitions: Map<Term, ConceptExpr>) (aBox: Assertions) : Assertions =
    let res =
        Set.map
            (fun assertion ->
                match assertion with
                | Assertion.Instance(i, c) ->
                    match c with
                    | ConceptExpr.AtomicConcept ac ->
                        match definitions.TryFind ac with
                        | Some c -> Assertion.Instance(i, c)
                        | None -> Assertion.Instance(i, c)
                    | ConceptExpr.And conj ->
                        let conj = List.map (fun value -> unfoldSingleExpression definitions value) conj
                        Assertion.Instance(i, ConceptExpr.And conj)
                    | ConceptExpr.Or(_) -> failwith "Not Implemented"
                    | ConceptExpr.Top -> failwith "Not Implemented"
                    | ConceptExpr.Bottom -> failwith "Not Implemented"
                    | ConceptExpr.Exists(_, _) -> failwith "Not Implemented"
                    | ConceptExpr.All(_, _) -> failwith "Not Implemented"
                    | ConceptExpr.Not c ->
                        let c = unfoldSingleExpression definitions c
                        Assertion.Instance(i, ConceptExpr.Not c)
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

type Interpretation(_definitions, _assertions) =
    let mutable current: IncompleteModel option = None
    let mutable incomplete: List<IncompleteModel> = []
    let mutable _model: Model option = None

    let rec handleTBox (tBox: Definitions) (aBox: Assertions) : Result<Assertions, LigatureError> =
        if tBox.IsEmpty then
            Ok aBox
        else if isDefinitorial tBox then
            match tBoxToMap tBox with
            | Some map -> Ok(unfoldTBox map aBox)
            | None -> error "Only definitorial TBoxes are supported currently." None
        else
            error "Only definitorial TBoxes are supported currently." None
    // let aBox' =
    //     if tBox.IsEmpty then
    //         aBox
    //     else
    //         Set.fold
    //             (fun state definition ->
    //                 match definition with
    //                 | Definition.Implies(ConceptExpr.AtomicConcept a, c) ->
    //                     match c with
    //                     | ConceptExpr.AtomicConcept c ->
    //                         Set.fold
    //                             (fun state value ->
    //                                 match value with
    //                                 | Assertion.Instance(ind, ConceptExpr.AtomicConcept concept) when concept = a ->
    //                                     Set.add (Assertion.Instance(ind, ConceptExpr.AtomicConcept c)) state
    //                                 | _ -> state)
    //                             state
    //                             aBox
    //                     | ConceptExpr.And conj ->
    //                         Set.fold
    //                             (fun state value ->
    //                                 match value with
    //                                 | Assertion.Instance(ind, ConceptExpr.AtomicConcept concept) when concept = a ->
    //                                     List.fold
    //                                         (fun state c -> Set.add (Assertion.Instance(ind, c)) state)
    //                                         state
    //                                         conj
    //                                 | _ -> state)
    //                             state
    //                             aBox
    //                     | ConceptExpr.Or(_) -> failwith "Not Implemented"
    //                     | ConceptExpr.Top -> failwith "Not Implemented"
    //                     | ConceptExpr.Bottom -> failwith "Not Implemented"
    //                     | ConceptExpr.Exists(_, _) -> failwith "Not Implemented"
    //                     | ConceptExpr.All(_, _) -> failwith "Not Implemented"
    //                     | ConceptExpr.Not(_) -> failwith "Not Implemented"
    //                 | Definition.Equivalent(a,c) ->
    //                     failwith "TODO")
    //             aBox
    //             tBox

    // if aBox <> aBox' then handleTBox tBox aBox' else aBox'

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

    let addRole (i: Term) (r: Term) (t: Term) =
        current <-
            Some
                { individuals = current.Value.individuals
                  assertions = current.Value.assertions
                  roles = Set.add (i, r, t) current.Value.roles
                  attributes = current.Value.attributes }

    let addAttribute (i: Term) (a: Term) (l: Literal) =
        current <-
            Some
                { individuals = current.Value.individuals
                  assertions = current.Value.assertions
                  roles = current.Value.roles
                  attributes = Set.add (i, a, l) current.Value.attributes }

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
        | Assertion.Instance(individual, ConceptExpr.AtomicConcept concept) ->
            let assertions = Set.remove assertion current.Value.assertions
            setAssertions assertions

            if assertions.IsEmpty then
                addInstance individual (Set.ofList [ concept ]) Set.empty
                succeed ()
            else
                addInstance individual (Set.ofList [ concept ]) Set.empty

        | Assertion.Instance(individual, ConceptExpr.And group) ->
            let mutable assertions = Set.remove assertion current.Value.assertions
            List.iter (fun expr -> assertions <- Set.add (Assertion.Instance(individual, expr)) assertions) group
            setAssertions assertions

            if assertions.IsEmpty then
                succeed ()
        | Assertion.Instance(individual, ConceptExpr.Or group) ->
            let mutable assertions = Set.remove assertion current.Value.assertions

            let alternatives: List<Assertions> =
                List.fold
                    (fun state expr -> Set.add (Assertion.Instance(individual, expr)) assertions :: state)
                    []
                    group

            setAlternatives alternatives

            if assertions.IsEmpty then
                succeed ()
        | Assertion.Instance(individual, ConceptExpr.All(role, concept)) ->
            let assertions = Set.remove assertion current.Value.assertions

            //TODO find all instances of the given role and mark all fillers as being `concept`
            let assertions =
                Set.fold
                    (fun state assertion ->
                        match assertion with
                        | Assertion.Triple(i, r, Value.Term f) when r = role && i = individual ->
                            Set.add (Assertion.Instance(f, concept)) state
                        | _ -> state)
                    assertions
                    assertions

            let assertions =
                Set.fold
                    (fun state value ->
                        match value with
                        | i, r, f when r = role && i = individual -> Set.add (Assertion.Instance(f, concept)) state
                        | _ -> state)
                    assertions
                    current.Value.roles

            setAssertions assertions
            //TODO handle inconsistent ConceptExprs
            if assertions.IsEmpty then
                succeed ()
        | Assertion.Instance(individual, ConceptExpr.Exists(_, _)) ->
            //TODO handle inconsistent ConceptExprs
            addInstance individual Set.empty Set.empty
            let assertions = Set.remove assertion current.Value.assertions
            setAssertions assertions

            if assertions.IsEmpty then
                succeed ()
        | Assertion.Instance(individual, ConceptExpr.Not(concept)) ->
            match concept with
            | ConceptExpr.AtomicConcept concept ->
                let assertions = Set.remove assertion current.Value.assertions
                setAssertions assertions
                addInstance individual Set.empty (Set.ofList [ concept ])

                if assertions.IsEmpty then
                    succeed ()
            | ConceptExpr.And group ->
                let mutable assertions = Set.remove assertion current.Value.assertions
                let negGroup = List.map (fun value -> ConceptExpr.Not value) group
                assertions <- Set.add (Assertion.Instance(individual, ConceptExpr.Or negGroup)) assertions
                setAssertions assertions
            | ConceptExpr.Or group ->
                let mutable assertions = Set.remove assertion current.Value.assertions
                let negGroup = List.map (fun value -> ConceptExpr.Not value) group
                assertions <- Set.add (Assertion.Instance(individual, ConceptExpr.And negGroup)) assertions
                setAssertions assertions
            | ConceptExpr.Top -> failwith "Not Implemented"
            | ConceptExpr.Bottom -> failwith "Not Implemented"
            | ConceptExpr.Exists(_, _) -> failwith "Not Implemented"
            | ConceptExpr.All(_, _) -> failwith "Not Implemented"
            | ConceptExpr.Not concept ->
                let assertions = Set.remove assertion current.Value.assertions
                let assertions = Set.add (Assertion.Instance(individual, concept)) assertions
                setAssertions assertions
        | Assertion.Triple(i, r, Value.Term t) ->
            let mutable assertions = Set.remove assertion current.Value.assertions
            setAssertions assertions
            addInstance i Set.empty Set.empty
            addInstance t Set.empty Set.empty
            addRole i r t

            if assertions.IsEmpty then
                succeed ()
        | Assertion.Triple(i, a, Value.Literal l) ->
            let mutable assertions = Set.remove assertion current.Value.assertions
            setAssertions assertions
            addInstance i Set.empty Set.empty
            addAttribute i a l

            if assertions.IsEmpty then
                succeed ()


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

    do
        match handleTBox _definitions _assertions with
        | Ok assertions -> current <- Some(newModel assertions)
        | _ -> failwith "TODO"

        processCurrent ()

        while _model = None && not incomplete.IsEmpty do
            incomplete <- incomplete.Tail
            current <- Some incomplete.Head
            processCurrent ()

    member _.model = _model

let findModel definitions assertions : Result<Model option, LigatureError> =
    let interpretation = Interpretation(definitions, assertions)
    Ok interpretation.model

let isConsistent definitions assertions : Result<bool, LigatureError> =
    let interpretation = new Interpretation(definitions, assertions)

    match interpretation.model with
    | None -> Ok false
    | Some interpretation ->
        Map.fold
            (fun state _ { isA = isA; isNot = isNot } ->
                match state with
                | Ok true ->
                    if (Set.intersect isA isNot).IsEmpty then
                        Ok true
                    else
                        Ok false
                | e -> e)
            (Ok true)
            interpretation.individuals
