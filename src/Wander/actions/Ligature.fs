// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Fns.Ligature

open Ligature.Model
open Ligature.Interpreter
open Ligature.Core
open Wander.Model
open Wander.Interpreter

[<RequireQualifiedAccess>]
type JsonViewValue =
    | Literal of string
    | Term of JsonView

and JsonView =
    { Id: string
      Attrs: Map<string, Set<JsonViewValue>> }

let rec writeValues (values: Set<JsonViewValue>) : string =
    Set.fold
        (fun state value ->
            let value =
                match value with
                | JsonViewValue.Term element -> writeJsonView element
                | JsonViewValue.Literal literal -> encodeString literal

            if state = "[" then state + value else state + "," + value)
        "["
        values
    + "]"

and writeJsonView (view: JsonView) : string =
    let mutable res = "{"

    res <- res + $"\"id\":{encodeString view.Id}"

    Map.iter (fun key values -> res <- res + $",{encodeString key}:{writeValues values}") view.Attrs

    res <- res + "}"
    res

let extract (id: Term) (source: Assertions) : Record =
    let mutable result = Map.empty
    result <- Map.add (Any.Term(Term "@")) (Any.Term id) result

    Set.iter
        (fun triple ->
            match triple with
            | Assertion.Triple(e, a, Value.Term v) ->
                if e = id then
                    result <- Map.add (Any.Term a) (Any.Term v) result
            | Assertion.Triple(e, a, Value.Literal v) ->
                if e = id then
                    result <- Map.add (Any.Term a) (Any.Literal v) result
            | Assertion.Instance(i, ConceptExpr.AtomicConcept c) ->
                if i = id then
                    result <- Map.add (Any.Term(Term ":")) (Any.Term c) result
            | _ -> failwith "TODO")
        source

    result

let extractFn: Fn =
    Fn(
        { doc = "Create a Record of a single invidual's relations."
          examples = [ "extract a (network [a b c])" ]
          args = "Term Network"
          result = "Record" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Term id; Any.Assertions source ] -> Ok(Any.Record(extract id source))
            | _ -> error "Invalid call to extract." None
    )

let instances (source: Assertions) (concept: Term) : AnySet =
    Set.fold
        (fun state triple ->
            match triple with
            | Assertion.Triple(element, Term ":", conceptToCheck) ->
                if conceptToCheck = Value.Term concept then
                    Set.add (Any.Record(extract element source)) state
                else
                    state
            | _ -> state)
        Set.empty
        source

let instancesFn: Fn =
    Fn(
        { doc = "..."
          examples = []
          args = ""
          result = "" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Term concept; Any.Assertions source ] ->
                let result: AnySet = instances source concept
                Ok(Any.AnySet result)
            | [ Any.Tuple concepts; Any.Assertions source ] ->
                let result: AnySet =
                    List.fold
                        (fun state concept ->
                            match concept with
                            | Any.Term concept -> instances source concept
                            | _ -> failwith "TODO")
                        Set.empty
                        concepts

                Ok(Any.AnySet result)
            | _ -> failwith "TODO"
    )

let isConsistentFn =
    Fn(
        { doc = "Check if a KB is consistent."
          examples = [ "(is-consistent (definitions (implies A B)) (assertions (instance a A)))" ]
          args = "Definitions Assertions"
          result = "Term" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Definitions def; Any.Assertions n ] ->
                match isConsistent def n with
                | Ok true -> Ok(Any.Term(Term "true"))
                | Ok false -> Ok(Any.Term(Term "false"))
                | Error err -> Error err
            | _ -> error "Network on stack required to call count." None
    )

// let inferFn: Fn =
//     Fn(
//         { doc = "..."
//           examples = []
//           args = ""
//           result = "" },
//         fun _ _ _ arguments ->
//             match arguments with
//             | [ description; network ] ->
//                 let description =
//                     match description with
//                     | Any.Definitions n -> n
//                     | _ -> failwith "TODO"

//                 let network =
//                     match network with
//                     | Any.Network n -> n
//                     | _ -> failwith "TODO"

//                 match infer description network with
//                 | Ok res -> Ok(Any.Network res)
//                 | Error err -> error $"Error calling infer: {err}" None
//             | _ -> error "Improper call to infer." None
//     )

let impliesFn: Fn =
    Fn(
        { doc = "Create a subconcept axiom."
          examples = [ "(implies Dog Animal)" ]
          args = ""
          result = "" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Term subconcept; Any.Term concept ] ->
                Ok(Any.Definition(Definition.Implies(subconcept, ConceptExpr.AtomicConcept concept)))
            | _ -> error "Improper call to implies." None
    )

let defineConceptFn: Fn =
    Fn(
        { doc = "Define a Concept."
          examples = [ "(define-concept Person (exists name))" ]
          args = ""
          result = "" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Term subconcept; Any.Term concept ] ->
                Ok(Any.Definition(Definition.Define(subconcept, ConceptExpr.AtomicConcept concept)))
            | [ Any.Term subconcept; Any.ConceptExpr concept ] ->
                Ok(Any.Definition(Definition.Define(subconcept, concept)))
            | _ -> error "Improper call to define-concept." None
    )

let findModelFn: Fn =
    Fn(
        { doc = "Find the first model that matches the given KB."
          examples = [ "(find-model (definitions) (assertions))" ]
          args = "Definitions Assertions"
          result = "Record" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Definitions definitions; Any.Assertions assertions ] ->
                match findModel definitions assertions with
                | Ok None -> Ok(Any.Record Map.empty)
                | Ok(Some model) ->

                    let individuals: Record =
                        Map.toSeq model.individuals
                        |> Seq.map (fun (key, { isA = isA; isNot = isNot }) ->
                            let isA = Set.map Any.Term isA |> Any.AnySet
                            let isNot = Set.map Any.Term isNot |> Any.AnySet

                            Any.Term key,
                            Any.Record(Map.ofList [ Any.Term(Term "is-a"), isA; Any.Term(Term "is-not"), isNot ])) //TODO this is wrong
                        |> Map.ofSeq

                    let roles: AnySet =
                        Set.map (fun (i, r, t) -> Any.Tuple [ Any.Term i; Any.Term r; Any.Term t ]) model.roles

                    let attributes: AnySet =
                        Set.map (fun (i, a, l) -> Any.Tuple [ Any.Term i; Any.Term a; Any.Literal l ]) model.attributes

                    Any.Record(
                        Map.ofList
                            [ Any.Term(Term "roles"), Any.AnySet roles
                              Any.Term(Term "individuals"), Any.Record individuals
                              Any.Term(Term "attributes"), Any.AnySet attributes ]
                    )
                    |> Ok
                | Error err -> Error err
            | _ -> error "Improper call to find-model." None
    )

let instanceFn: Fn =
    Fn(
        { doc = "Assert an Individual extends a Concept."
          examples = [ "(instance betty (and Cat (not Dog)))" ]
          args = ""
          result = "" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Term individual; Any.Term concept ] ->
                Ok(Any.Assertion(Assertion.Instance(individual, ConceptExpr.AtomicConcept concept)))
            | [ Any.Term individual; Any.ConceptExpr concept ] ->
                Ok(Any.Assertion(Assertion.Instance(individual, concept)))
            | _ -> error "Improper call to instance." None
    )

let allFn: Fn =
    Fn(
        { doc = "Create a ∀ Concept."
          examples = [ "(all knows Person)" ]
          args = ""
          result = "" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Term role; Any.Term concept ] ->
                Ok(Any.ConceptExpr(ConceptExpr.All(role, ConceptExpr.AtomicConcept concept)))
            | [ Any.Term role; Any.ConceptExpr concept ] -> Ok(Any.ConceptExpr(ConceptExpr.All(role, concept)))
            | _ -> error "Improper call to all." None
    )

let existsFn: Fn =
    Fn(
        { doc = "Create an ∃ Concept."
          examples = [ "(exists knows Person)" ]
          args = ""
          result = "" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Term role; Any.Term concept ] ->
                Ok(Any.ConceptExpr(ConceptExpr.Exists(role, ConceptExpr.AtomicConcept concept)))
            | [ Any.Term role; Any.ConceptExpr concept ] -> Ok(Any.ConceptExpr(ConceptExpr.Exists(role, concept)))
            | _ -> error "Improper call to exists." None
    )

let notFn: Fn =
    Fn(
        { doc = "Negate a Concept Expression."
          examples = [ "(not Dog)"; "(not (and Cat Dog))" ]
          args = "ConceptExpression"
          result = "ConceptExpression" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Term concept ] -> Ok(Any.ConceptExpr(ConceptExpr.Not(ConceptExpr.AtomicConcept concept)))
            | [ Any.ConceptExpr concept ] -> Ok(Any.ConceptExpr(ConceptExpr.Not(concept)))
            | _ -> error "Improper call to not." None
    )

let andFn: Fn =
    Fn(
        { doc = "And multiple Concept Expressions."
          examples = [ "(and Cat Dog Ferret)"; "(and Cat (not (and Ferret Dog))))" ]
          args = "ConceptExpression"
          result = "ConceptExpression" },
        fun _ _ _ arguments ->
            let res =
                List.fold
                    (fun state arg ->
                        match state with
                        | Ok state ->
                            match arg with
                            | Any.Term term -> Ok(List.append state [ ConceptExpr.AtomicConcept term ])
                            | Any.ConceptExpr expr -> Ok(List.append state [ expr ])
                            | _ -> error "Invalid argument." None
                        | _ -> state)
                    (Ok [])
                    arguments

            match res with
            | Ok value -> Ok(Any.ConceptExpr(ConceptExpr.And value))
            | Error err -> Error err
    )

let orFn: Fn =
    Fn(
        { doc = "Or multiple Concept Expressions."
          examples = [ "(or Cat Dog Ferret)" ]
          args = "ConceptExpression"
          result = "ConceptExpression" },
        fun _ _ _ arguments ->
            let res =
                List.fold
                    (fun state arg ->
                        match state with
                        | Ok state ->
                            match arg with
                            | Any.Term term -> Ok(List.append state [ ConceptExpr.AtomicConcept term ])
                            | Any.ConceptExpr expr -> Ok(List.append state [ expr ])
                            | _ -> error "Invalid argument." None
                        | _ -> state)
                    (Ok [])
                    arguments

            match res with
            | Ok value -> Ok(Any.ConceptExpr(ConceptExpr.Or value))
            | Error err -> Error err
    )

let definitionsFn: Fn =
    Fn(
        { doc = "Define a TBox."
          examples = [ "(definitions (implies Dog Animal))" ]
          args = ""
          result = "" },
        fun _ _ _ arguments ->
            List.fold
                (fun state value ->
                    match state with
                    | Ok(Any.Definitions state) ->
                        match value with
                        | Any.Definition def -> Ok(Any.Definitions(Set.add def state))
                        | _ -> failwith "TODO"
                    | Ok _ -> failwith "Unexpected value."
                    | Error err -> Error err)
                (Ok(Any.Definitions Set.empty))
                arguments
    )
