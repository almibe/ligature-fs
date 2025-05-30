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

// let extract (id: Term) (source: Assertions) : Record =
//     let mutable result = Map.empty
//     result <- Map.add (Any.Term(Term "@")) (Any.Term id) result

//     Set.iter
//         (fun triple ->
//             match triple with
//             | Assertion.Triple(e, a, Value.Term v) ->
//                 if e = id then
//                     result <- Map.add (Any.Term a) (Any.Term v) result
//             | Assertion.Triple(e, a, Value.Literal v) ->
//                 if e = id then
//                     result <- Map.add (Any.Term a) (Any.Literal v) result
//             | Assertion.Instance(i, ConceptExpr.AtomicConcept c) ->
//                 if i = id then
//                     result <- Map.add (Any.Term(Term ":")) (Any.Term c) result
//             | _ -> failwith "TODO")
//         source

//     result

// let extractFn: Fn =
//     Fn(
//         { doc = "Create a Record of a single invidual's relations."
//           examples = [ "extract a (network [a b c])" ]
//           args = "Term Network"
//           result = "Record" },
//         fun _ _ _ arguments ->
//             match arguments with
//             | [ Any.Term id; Any.Assertions source ] -> Ok(Any.Node(extract id source))
//             | _ -> error "Invalid call to extract." None
//     )

// let instances (source: Assertions) (concept: Term) : AnySet =
//     Set.fold
//         (fun state triple ->
//             match triple with
//             | Assertion.Triple(element, Term ":", conceptToCheck) ->
//                 if conceptToCheck = Value.Term concept then
//                     Set.add (Any.Node(extract element source)) state
//                 else
//                     state
//             | _ -> state)
//         Set.empty
//         source

// let instancesFn: Fn =
//     Fn(
//         { doc = "..."
//           examples = []
//           args = ""
//           result = "" },
//         fun _ _ _ arguments ->
//             match arguments with
//             | [ Any.Term concept; Any.Assertions source ] ->
//                 let result: AnySet = instances source concept
//                 Ok(Any.AnySet result)
//             | [ Any.Tuple concepts; Any.Assertions source ] ->
//                 let result: AnySet =
//                     List.fold
//                         (fun state concept ->
//                             match concept with
//                             | Any.Term concept -> instances source concept
//                             | _ -> failwith "TODO")
//                         Set.empty
//                         concepts

//                 Ok(Any.AnySet result)
//             | _ -> failwith "TODO"
//     )

let unfoldFn =
    Fn(
        { doc = "Unfold a TBox into an ABox."
          examples = [ "(unfold (definitions (implies A B)) (assertions [a : A]))" ]
          args = "Definitions Assertions"
          result = "Assertions" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.TBox def; Any.ABox assertions ] -> unfold def assertions |> Result.map Any.ABox
            | _ -> error "Invalid call to unfold." None
    )

let isDefinitorialFn =
    Fn(
        { doc = "Check if definitions are definitorial."
          examples = [ "(is-definitorial (definitions (implies A B)))" ]
          args = "Definitions"
          result = "Literal" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.TBox def ] ->
                if isDefinitorial def then
                    Ok(Any.Term(Term "true"))
                else
                    Ok(Any.Term(Term "false"))
            | _ -> error "Invalid call to is-definitorial." None
    )

let nnfFn =
    Fn(
        { doc = "Convert definitions to nnf."
          examples = [ "(nnf (definitions (implies A B)))" ]
          args = "Definitions"
          result = "Definitions" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.TBox def ] ->
                match nnf def with
                | Ok resultValue -> Ok(Any.ConceptExpr resultValue)
                | Error err -> Error err
            | _ -> error "Invalid call to nnf." None
    )

let bottomFn =
    Fn(
        { doc = "Constructor for bottom concept."
          examples = [ "(bottom)" ]
          args = ""
          result = "ConceptExpr" },
        fun _ _ _ arguments ->
            match arguments with
            | [] -> Ok(Any.ConceptExpr ConceptExpr.Bottom)
            | _ -> error "Invalid call to bottom." None
    )


let topFn =
    Fn(
        { doc = "Constructor for top concept."
          examples = [ "(top)" ]
          args = ""
          result = "ConceptExpr" },
        fun _ _ _ arguments ->
            match arguments with
            | [] -> Ok(Any.ConceptExpr ConceptExpr.Top)
            | _ -> error "Invalid call to bottom." None
    )

let isConsistentFn =
    Fn(
        { doc = "Check if a KB is consistent."
          examples = [ "(is-consistent (definitions (implies A B)) (assertions (instance a A)))" ]
          args = "Definitions Assertions"
          result = "Term" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.TBox def; Any.ABox n ] ->
                match isConsistent def n with
                | Ok true -> Ok(Any.Term(Term "true"))
                | Ok false -> Ok(Any.Term(Term "false"))
                | Error err -> Error err
            | _ -> error "Invalid call to is-consistent." None
    )

let isInstanceFn =
    Fn(
        { doc = "Check if an individual is an instance of a concept."
          examples = [ "(is-istance (definitions (implies A B)) (assertions (instance a A)) a B)" ]
          args = "Definitions Assertions Individual Concept"
          result = "Term" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.TBox tBox; Any.ABox aBox; Any.Term individual; Any.Term concept ] -> //TODO handle conceptexprs
                match isInstance tBox aBox individual concept with
                | Ok term -> Ok(Any.Term term)
                | Error err -> Error err
            | _ -> error "Invalid call to is-consistent." None
    )

let impliesFn: Fn =
    Fn(
        { doc = "Create a subconcept axiom."
          examples = [ "(implies Dog Animal)" ]
          args = ""
          result = "" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Term subconcept; Any.Term concept ] ->
                Ok(
                    Any.ConceptExpr(
                        ConceptExpr.Implies(ConceptExpr.AtomicConcept subconcept, ConceptExpr.AtomicConcept concept)
                    )
                )
            | [ Any.Term subconcept; Any.ConceptExpr concept ] ->
                Ok(Any.ConceptExpr(ConceptExpr.Implies(ConceptExpr.AtomicConcept subconcept, concept)))
            | _ -> error "Improper call to implies." None
    )

let equivalentFn: Fn =
    Fn(
        { doc = "State two Concepts are equivs."
          examples = [ "(equiv Named (exists name))" ]
          args = ""
          result = "" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Term subconcept; Any.Term concept ] ->
                Ok(
                    Any.ConceptExpr(
                        ConceptExpr.Equivalent(ConceptExpr.AtomicConcept subconcept, ConceptExpr.AtomicConcept concept)
                    )
                )
            | [ Any.Term subconcept; Any.ConceptExpr concept ] ->
                Ok(Any.ConceptExpr(ConceptExpr.Equivalent(ConceptExpr.AtomicConcept subconcept, concept)))
            | _ -> error "Improper call to define-concept." None
    )

let findModelFn: Fn =
    Fn(
        { doc = "Find the first model that matches the given KB."
          examples = [ "(find-model (definitions) (assertions))" ]
          args = "Definitions Assertions"
          result = "Record" },
        fun _ _ _ arguments -> failwith "TODO"
    )
// match arguments with
// | [ Any.Definitions definitions; Any.Assertions assertions ] ->
//     match findModel definitions assertions with
//     | Ok None -> Ok(Any.Tuple [])
//     | Ok(Some model) ->

//         let individuals: Node =

//             Map.toSeq model.individuals
//             |> Seq.map (fun (key, { isA = isA; isNot = isNot }) ->
//                 let isA = Set.map Any.Term isA |> Any.AnySet
//                 let isNot = Set.map Any.Term isNot |> Any.AnySet

//                 Any.Term key,
//                 Any.Node(Map.ofList [ Any.Term(Term "is-a"), isA; Any.Term(Term "is-not"), isNot ])) //TODO this is wrong
//             |> Map.ofSeq

//         let roles: AnySet =
//             Set.map (fun (i, r, t) -> Any.Tuple [ Any.Term i; Any.Term r; Any.Term t ]) model.roles

//         let attributes: AnySet =
//             Set.map (fun (i, a, l) -> Any.Tuple [ Any.Term i; Any.Term a; Any.Literal l ]) model.attributes

//         // Any.Node(
//         //     Map.ofList
//         //         [ Any.Term(Term "roles"), Any.AnySet roles
//         //           //Any.Term(Term "individuals"), Any.Node individuals
//         //           Any.Term(Term "attributes"), Any.AnySet attributes ]
//         // )
//         // |> Ok
//         failwith "TODO"
//     | Error err -> Error err
// | _ -> error "Improper call to find-model." None)

let literalFn: Fn =
    Fn(
        { doc = "Create a literal."
          examples = [ "(literal \"# hello\" Markdown en)" ]
          args = "Literal Term Term"
          result = "Literal" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Literal { content = content }; Any.Term datatype; Any.Term(Term langTag) ] ->
                Ok(
                    Any.Literal
                        { content = content
                          datatype = Some datatype
                          langTag = Some langTag }
                )
            | _ -> error "Improper call to literal." None
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

let conceptFn: Fn =
    Fn(
        { doc = "Convert a term to an atomic concept."
          examples = [ "(concept A)" ]
          args = "Term"
          result = "Concept" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Term concept ] -> Ok(Any.ConceptExpr(ConceptExpr.AtomicConcept concept))
            | _ -> error "Improper call to concept." None
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
          examples = [ "(exists name) (exists knows Person)" ]
          args = "RoleName Concept?"
          result = "Concept" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Term role ] -> Ok(Any.ConceptExpr(ConceptExpr.Exists(role, ConceptExpr.Top)))
            | [ Any.Term role; Any.Term concept ] ->
                Ok(Any.ConceptExpr(ConceptExpr.Exists(role, ConceptExpr.AtomicConcept concept)))
            | [ Any.Term role; Any.ConceptExpr concept ] -> Ok(Any.ConceptExpr(ConceptExpr.Exists(role, concept)))
            | _ -> error "Improper call to exists." None
    )

let exactlyFn: Fn =
    Fn(
        { doc = "Create a numerical restriction with an exact requirement."
          examples = [ "(exactly 1 first-name)"; "(exactly 1 first-name Literal)" ]
          args = "Int RoleName Concept?"
          result = "Concept" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Term(Term number); Any.Term role ] ->
                match System.Int64.TryParse number with
                | true, number -> Ok(Any.ConceptExpr(ConceptExpr.Exactly(role, ConceptExpr.Top, number)))
                | _ -> error "Improper call to exactly." None
            | [ Any.Term(Term number); Any.Term role; Any.Term concept ] ->
                match System.Int64.TryParse number with
                | true, number ->
                    Ok(Any.ConceptExpr(ConceptExpr.Exactly(role, ConceptExpr.AtomicConcept concept, number)))
                | _ -> error "Improper call to exactly." None
            | [ Any.Term(Term number); Any.Term role; Any.ConceptExpr concept ] ->
                match System.Int64.TryParse number with
                | true, number -> Ok(Any.ConceptExpr(ConceptExpr.Exactly(role, concept, number)))
                | _ -> error "Improper call to exactly." None
            | _ -> error "Improper call to exactly." None
    )

let atLeastFn: Fn =
    Fn(
        { doc = "Create a numerical restriction with an at least requirement."
          examples = [ "(at-least 1 email)"; "(at-least 1 email EmailAddress)" ]
          args = "Int RoleName Concept?"
          result = "Concept" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Term(Term number); Any.Term role ] ->
                match System.Int64.TryParse number with
                | true, number -> Ok(Any.ConceptExpr(ConceptExpr.AtLeast(role, ConceptExpr.Top, number)))
                | _ -> error "Improper call to at-least." None
            | [ Any.Term(Term number); Any.Term role; Any.Term concept ] ->
                match System.Int64.TryParse number with
                | true, number ->
                    Ok(Any.ConceptExpr(ConceptExpr.AtLeast(role, ConceptExpr.AtomicConcept concept, number)))
                | _ -> error "Improper call to at-least." None
            | [ Any.Term(Term number); Any.Term role; Any.ConceptExpr concept ] ->
                match System.Int64.TryParse number with
                | true, number -> Ok(Any.ConceptExpr(ConceptExpr.AtLeast(role, concept, number)))
                | _ -> error "Improper call to at-least." None
            | _ -> error "Improper call to at-least." None
    )

let atMostFn: Fn =
    Fn(
        { doc = "Create a numerical restriction with an at most requirement."
          examples = [ "(at-most 6 pinned-posts)"; "(at-most 6 pinned-posts Post)" ]
          args = "Int RoleName Concept?"
          result = "Concept" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Term(Term number); Any.Term role ] ->
                match System.Int64.TryParse number with
                | true, number -> Ok(Any.ConceptExpr(ConceptExpr.AtMost(role, ConceptExpr.Top, number)))
                | _ -> error "Improper call to at-most." None
            | [ Any.Term(Term number); Any.Term role; Any.Term concept ] ->
                match System.Int64.TryParse number with
                | true, number ->
                    Ok(Any.ConceptExpr(ConceptExpr.AtMost(role, ConceptExpr.AtomicConcept concept, number)))
                | _ -> error "Improper call to at-most." None
            | [ Any.Term(Term number); Any.Term role; Any.ConceptExpr concept ] ->
                match System.Int64.TryParse number with
                | true, number -> Ok(Any.ConceptExpr(ConceptExpr.AtMost(role, concept, number)))
                | _ -> error "Improper call to at-most." None
            | _ -> error "Improper call to at-most." None
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

let tBoxFn: Fn =
    Fn(
        { doc = "Define a TBox."
          examples = [ "(t-box (implies Dog Animal))" ]
          args = ""
          result = "" },
        fun _ _ _ arguments ->
            List.map
                (fun value ->
                    match value with
                    | Any.ConceptExpr expr -> expr
                    | Any.Term term -> ConceptExpr.AtomicConcept term
                    | _ -> failwith "Not suported.")
                arguments
            |> Any.TBox
            |> Ok
    )
