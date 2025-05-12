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

type PotentialModel =
    { mutable assertions: Assertions //TODO probably make this a list too
      mutable individuals: Map<Term, ConceptValues>
      mutable roles: Set<Term * Term * Term>
      mutable attributes: Set<Term * Term * Literal> }

let newModel assertions =
    { assertions = assertions
      individuals = Map.empty
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
        | ConceptExpr.Or disj ->
            List.forall (fun value -> not (hasCycle definitionsMap concept value)) disj
            |> not
        | ConceptExpr.Top -> false
        | ConceptExpr.Bottom -> false
        | ConceptExpr.Exists(roleName, c) -> hasCycle definitionsMap concept c
        | ConceptExpr.All(roleName, c) -> hasCycle definitionsMap concept c

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
    | ConceptExpr.Exists(roleName, c) -> unfoldSingleExpression definitions c
    | ConceptExpr.All(roleName, c) -> unfoldSingleExpression definitions c
    | ConceptExpr.Not c ->
        let c = unfoldSingleExpression definitions c
        ConceptExpr.Not c

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

type Interpretation(_definitions, _assertions) =
    let mutable current: PotentialModel option = None
    let mutable incomplete: List<PotentialModel> = []
    let mutable _model: Model option = None

    let setAssertions (assertions: Assertions) = current.Value.assertions <- assertions

    let setAlternatives (alternatives: List<Assertions>) =
        match alternatives with
        | [] -> ()
        | [ single ] -> current.Value.assertions <- single
        | head :: tail ->
            current.Value.assertions <- head

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
        current.Value.roles <- Set.add (i, r, t) current.Value.roles

    let addAttribute (i: Term) (a: Term) (l: Literal) =
        current.Value.attributes <- Set.add (i, a, l) current.Value.attributes

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

        current.Value.individuals <- individuals

    let isConsistent (model: PotentialModel) : bool =
        Map.fold
            (fun state _ { isA = isA; isNot = isNot } ->
                match state with
                | true -> if (Set.intersect isA isNot).IsEmpty then true else false
                | e -> e)
            true
            model.individuals

    let complete () =
        if isConsistent current.Value then
            _model <-
                Some
                    { individuals = current.Value.individuals
                      attributes = current.Value.attributes
                      roles = current.Value.roles }

            current <- None
        else
            current <- None

    let interpretNextAssertion () =
        if current.Value.assertions.IsEmpty then
            if incomplete.IsEmpty then
                current <- None
            else
                current <- Some incomplete.Head
                incomplete <- incomplete.Tail
        else
            let assertion = current.Value.assertions.MinimumElement

            match assertion with
            | Assertion.Instance(individual, ConceptExpr.AtomicConcept concept) ->
                let assertions = Set.remove assertion current.Value.assertions
                setAssertions assertions

                if assertions.IsEmpty then
                    addInstance individual (Set.ofList [ concept ]) Set.empty
                    complete ()
                else
                    addInstance individual (Set.ofList [ concept ]) Set.empty

            | Assertion.Instance(individual, ConceptExpr.And group) ->
                let mutable assertions = Set.remove assertion current.Value.assertions
                List.iter (fun expr -> assertions <- Set.add (Assertion.Instance(individual, expr)) assertions) group
                setAssertions assertions

                if assertions.IsEmpty then
                    complete ()
            | Assertion.Instance(individual, ConceptExpr.Or group) ->
                let mutable assertions = Set.remove assertion current.Value.assertions

                let alternatives: List<Assertions> =
                    List.fold
                        (fun state expr -> Set.add (Assertion.Instance(individual, expr)) assertions :: state)
                        []
                        group

                setAlternatives alternatives

                if current.Value.assertions.IsEmpty then
                    complete ()
            | Assertion.Instance(individual, ConceptExpr.All(role, concept)) ->
                if not (Set.exists (fun value -> match value with | Assertion.Triple _ -> true | _ -> false) current.Value.assertions) then
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
                        complete ()
                else
                    () //wait to process
            | Assertion.Instance(individual, ConceptExpr.Exists(_, _)) ->
                if not (Set.exists (fun value -> match value with | Assertion.Triple _ -> true | _ -> false) current.Value.assertions) then
                    //TODO handle inconsistent ConceptExprs
                    addInstance individual Set.empty Set.empty
                    let assertions = Set.remove assertion current.Value.assertions
                    setAssertions assertions

                    if assertions.IsEmpty then
                        complete ()
                else
                    () //wait to process
            | Assertion.Instance(individual, ConceptExpr.Not(concept)) ->
                match concept with
                | ConceptExpr.AtomicConcept concept ->
                    let assertions = Set.remove assertion current.Value.assertions
                    setAssertions assertions
                    addInstance individual Set.empty (Set.ofList [ concept ])

                    if assertions.IsEmpty then
                        complete ()
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
                | ConceptExpr.Top -> setAssertions (Set.remove assertion current.Value.assertions)
                | ConceptExpr.Bottom -> setAssertions (Set.remove assertion current.Value.assertions)
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
                    complete ()
            | Assertion.Triple(i, a, Value.Literal l) ->
                let mutable assertions = Set.remove assertion current.Value.assertions
                setAssertions assertions
                addInstance i Set.empty Set.empty
                addAttribute i a l

                if assertions.IsEmpty then
                    complete ()

    do
        match handleTBox _definitions _assertions with
        | Ok assertions -> current <- Some(newModel assertions)
        | _ -> failwith "TODO"

        interpretNextAssertion ()

        while _model = None && (current.IsSome || not incomplete.IsEmpty) do
            if current.IsNone then
                current <- Some incomplete.Head
                incomplete <- incomplete.Tail
                interpretNextAssertion ()
            else
                interpretNextAssertion ()

    member _.model = _model

let findModel definitions assertions : Result<Model option, LigatureError> =
    let interpretation = Interpretation(definitions, assertions)
    Ok interpretation.model

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
        | ConceptExpr.Implies(lhs, rhs) -> 
            ConceptExpr.Or [ConceptExpr.Not lhs; rhs]
        | ConceptExpr.Equivalent(lhs, rhs) -> 
            ConceptExpr.And [
                ConceptExpr.Or [ConceptExpr.Not lhs; rhs]
                ConceptExpr.Or [ConceptExpr.Not rhs; lhs]
            ]

    and nnf (definitions': Definitions) : ConceptExpr =
        if not definitions'.IsEmpty then
            List.map (fun value -> nnfConcept value) definitions' |> ConceptExpr.And
        else
            ConceptExpr.Top

    Ok(nnf definitions)

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
