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

// let extract (id: Term) (source: Assertions) : Node =
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
//         { doc = "Create a Node of a single invidual's relations."
//           examples = [ "extract a (network [a b c])" ]
//           args = "Term Network"
//           result = "Node" },
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
            | [ Expression.Definitions def; Expression.Assertions assertions ] ->
                unfold def assertions |> Result.map Expression.Assertions
            | _ -> error "Invalid call to unfold." None
    )

let isDefinitorialFn =
    Fn(
        { doc = "Check if t-box is definitorial."
          examples = [ "(is-definitorial (definitions (implies A B)))" ]
          args = "Definitions"
          result = "Literal" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Expression.Definitions def ] ->
                if isDefinitorial def then
                    Ok(Expression.Term(Term "true"))
                else
                    Ok(Expression.Term(Term "false"))
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
            | [ Expression.Definitions def ] ->
                match nnf def with
                | Ok resultValue -> Ok(Expression.ConceptExpr resultValue)
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
            | [] -> Ok(Expression.ConceptExpr ConceptExpr.Bottom)
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
            | [] -> Ok(Expression.ConceptExpr ConceptExpr.Top)
            | _ -> error "Invalid call to bottom." None
    )

let isInstanceFn =
    Fn(
        { doc = "Check if an instance is a concept."
          examples = [ "(is-instance (definitions (implies A B)) (assertions (instance a A)) a B)" ]
          args = "Definitions Assertions Individual Concept"
          result = "Term" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Expression.Definitions tBox
                Expression.Assertions aBox
                Expression.Term element
                Expression.Term concept ] -> //TODO handle conceptexprs
                match isInstance tBox aBox (termToElement element) (ConceptExpr.AtomicConcept concept) with
                | Ok term -> Ok(Expression.Term term)
                | Error err -> Error err
            | [ Expression.Definitions tBox
                Expression.Assertions aBox
                Expression.Term element
                Expression.ConceptExpr concept ] -> //TODO handle conceptexprs
                match isInstance tBox aBox (termToElement element) concept with
                | Ok term -> Ok(Expression.Term term)
                | Error err -> Error err
            | _ -> error "Invalid call to is-instance." None
    )

let impliesFn: Fn =
    Fn(
        { doc = "Create a subconcept axiom."
          examples = [ "(implies Dog Animal)" ]
          args = ""
          result = "" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Expression.Term subconcept; Expression.Term concept ] ->
                Ok(
                    Expression.Definition(
                        Definition.Implies(ConceptExpr.AtomicConcept subconcept, ConceptExpr.AtomicConcept concept)
                    )
                )
            | [ Expression.Term subconcept; Expression.ConceptExpr concept ] ->
                Ok(Expression.Definition(Definition.Implies(ConceptExpr.AtomicConcept subconcept, concept)))
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
            | [ Expression.Term leftConcept; Expression.Term rightConcept ] ->
                Ok(
                    Expression.Definition(
                        Definition.Equivalent(
                            ConceptExpr.AtomicConcept leftConcept,
                            ConceptExpr.AtomicConcept rightConcept
                        )
                    )
                )
            | [ Expression.Term leftConcept; Expression.ConceptExpr rightConcept ] ->
                Ok(Expression.Definition(Definition.Equivalent(ConceptExpr.AtomicConcept leftConcept, rightConcept)))
            | _ -> error "Improper call to equivalent." None
    )

let tableauModelsFn: Fn =
    Fn(
        { doc =
            "Find all models using the tableau algorithm and return them based on whether or not they contain clashes."
          examples = [ "(tableau-models (definitions) (assertions))" ]
          args = "Definitions Assertions"
          result = "Set" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Expression.Definitions tBox; Expression.Assertions aBox ] ->
                match tableauModels tBox aBox with
                | Ok res ->
                    List.map (fun value -> Expression.Assertions value) res
                    |> Set.ofList
                    |> Expression.Set
                    |> Ok
                | Error err -> Error err
            | _ -> error "Invalid call to tableau-model." None
    )

// let findModelFn: Fn =
//     Fn(
//         { doc = "Find the first model that matches the given KB."
//           examples = [ "(find-model (definitions) (assertions))" ]
//           args = "Definitions Assertions"
//           result = "Node" },
//         fun _ _ _ arguments -> failwith "TODO"
//     )
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

let elementFn: Fn =
    Fn(
        { doc = "Create an element."
          examples = [ "(element \"# hello\" Markdown en)" ]
          args = "Literal Term Term"
          result = "Individual" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Expression.Element { value = content }; Expression.Term datatype; Expression.Term(Term langTag) ] ->
                Ok(
                    Expression.Element
                        { value = content
                          space = Some datatype
                          langTag = Some langTag }
                )
            | _ -> error "Improper call to element." None
    )

let instanceFn: Fn =
    Fn(
        { doc = "Assert an Individual extends a Concept."
          examples = [ "(instance betty (and Cat (not Dog)))" ]
          args = ""
          result = "" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Expression.Term(Term element); Expression.Term concept ] ->
                Ok(
                    Expression.Assertion(
                        Assertion.Instance(
                            { value = element
                              space = None
                              langTag = None },
                            ConceptExpr.AtomicConcept concept
                        )
                    )
                )
            | [ Expression.Term(Term element); Expression.ConceptExpr concept ] ->
                Ok(
                    Expression.Assertion(
                        Assertion.Instance(
                            { value = element
                              space = None
                              langTag = None },
                            concept
                        )
                    )
                )
            | [ Expression.Element instance; Expression.Term concept ] ->
                Ok(Expression.Assertion(Assertion.Instance(instance, ConceptExpr.AtomicConcept concept)))
            | [ Expression.Element instance; Expression.ConceptExpr concept ] ->
                Ok(Expression.Assertion(Assertion.Instance(instance, concept)))
            | x -> error $"Improper call to instance: {x}" None
    )

let sameFn: Fn =
    Fn(
        { doc = "Assert two names reference the same Individual."
          examples = [ "(same a b)" ]
          args = "Individual Individual"
          result = "Assertion" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Expression.Term left; Expression.Term right ] ->
                Ok(Expression.Assertion(Assertion.Same(termToElement left, termToElement right)))
            | [ Expression.Element left; Expression.Element right ] ->
                Ok(Expression.Assertion(Assertion.Same(left, right)))
            | _ -> error "Improper call to same." None
    )

let differentFn: Fn =
    Fn(
        { doc = "Assert two names reference different Individuals."
          examples = [ "(different a b)" ]
          args = "Individual Individual"
          result = "Assertion" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Expression.Term left; Expression.Term right ] ->
                Ok(Expression.Assertion(Assertion.Different(termToElement left, termToElement right)))
            | [ Expression.Element left; Expression.Element right ] ->
                Ok(Expression.Assertion(Assertion.Different(left, right)))
            | _ -> error "Improper call to different." None
    )

let conceptFn: Fn =
    Fn(
        { doc = "Convert a term to an atomic concept."
          examples = [ "(concept A)" ]
          args = "Term"
          result = "Concept" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Expression.Term concept ] -> Ok(Expression.ConceptExpr(ConceptExpr.AtomicConcept concept))
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
            | [ Expression.Term role; Expression.Term concept ] ->
                Ok(Expression.ConceptExpr(ConceptExpr.All(role, ConceptExpr.AtomicConcept concept)))
            | [ Expression.Term role; Expression.ConceptExpr concept ] ->
                Ok(Expression.ConceptExpr(ConceptExpr.All(role, concept)))
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
            | [ Expression.Term role ] -> Ok(Expression.ConceptExpr(ConceptExpr.Exists(role, ConceptExpr.Top)))
            | [ Expression.Term role; Expression.Term concept ] ->
                Ok(Expression.ConceptExpr(ConceptExpr.Exists(role, ConceptExpr.AtomicConcept concept)))
            | [ Expression.Term role; Expression.ConceptExpr concept ] ->
                Ok(Expression.ConceptExpr(ConceptExpr.Exists(role, concept)))
            | _ -> error "Improper call to exists." None
    )

let funcFn: Fn =
    Fn(
        { doc = "Create a function role definition."
          examples = [ "(func name)" ]
          args = "RoleName"
          result = "Concept" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Expression.Term role ] -> Ok(Expression.ConceptExpr(ConceptExpr.Func role))
            | _ -> error "Improper call to func." None
    )

// let exactlyFn: Fn =
//     Fn(
//         { doc = "Create a numerical restriction with an exact requirement."
//           examples = [ "(exactly 1 first-name)"; "(exactly 1 first-name Literal)" ]
//           args = "Int RoleName Concept?"
//           result = "Concept" },
//         fun _ _ _ arguments ->
//             match arguments with
//             | [ Expression.Term(Term number); Expression.Term role ] ->
//                 match System.Int64.TryParse number with
//                 | true, number -> Ok(Expression.ConceptExpr(ConceptExpr.Exactly(role, ConceptExpr.Top, number)))
//                 | _ -> error "Improper call to exactly." None
//             | [ Expression.Term(Term number); Expression.Term role; Expression.Term concept ] ->
//                 match System.Int64.TryParse number with
//                 | true, number ->
//                     Ok(Expression.ConceptExpr(ConceptExpr.Exactly(role, ConceptExpr.AtomicConcept concept, number)))
//                 | _ -> error "Improper call to exactly." None
//             | [ Expression.Term(Term number); Expression.Term role; Expression.ConceptExpr concept ] ->
//                 match System.Int64.TryParse number with
//                 | true, number -> Ok(Expression.ConceptExpr(ConceptExpr.Exactly(role, concept, number)))
//                 | _ -> error "Improper call to exactly." None
//             | _ -> error "Improper call to exactly." None
//     )

// let atLeastFn: Fn =
//     Fn(
//         { doc = "Create a numerical restriction with an at least requirement."
//           examples = [ "(at-least 1 email)"; "(at-least 1 email EmailAddress)" ]
//           args = "Int RoleName Concept?"
//           result = "Concept" },
//         fun _ _ _ arguments ->
//             match arguments with
//             | [ Expression.Term(Term number); Expression.Term role ] ->
//                 match System.Int64.TryParse number with
//                 | true, number -> Ok(Expression.ConceptExpr(ConceptExpr.AtLeast(role, ConceptExpr.Top, number)))
//                 | _ -> error "Improper call to at-least." None
//             | [ Expression.Term(Term number); Expression.Term role; Expression.Term concept ] ->
//                 match System.Int64.TryParse number with
//                 | true, number ->
//                     Ok(Expression.ConceptExpr(ConceptExpr.AtLeast(role, ConceptExpr.AtomicConcept concept, number)))
//                 | _ -> error "Improper call to at-least." None
//             | [ Expression.Term(Term number); Expression.Term role; Expression.ConceptExpr concept ] ->
//                 match System.Int64.TryParse number with
//                 | true, number -> Ok(Expression.ConceptExpr(ConceptExpr.AtLeast(role, concept, number)))
//                 | _ -> error "Improper call to at-least." None
//             | _ -> error "Improper call to at-least." None
//     )

// let atMostFn: Fn =
//     Fn(
//         { doc = "Create a numerical restriction with an at most requirement."
//           examples = [ "(at-most 6 pinned-posts)"; "(at-most 6 pinned-posts Post)" ]
//           args = "Int RoleName Concept?"
//           result = "Concept" },
//         fun _ _ _ arguments ->
//             match arguments with
//             | [ Expression.Term(Term number); Expression.Term role ] ->
//                 match System.Int64.TryParse number with
//                 | true, number -> Ok(Expression.ConceptExpr(ConceptExpr.AtMost(role, ConceptExpr.Top, number)))
//                 | _ -> error "Improper call to at-most." None
//             | [ Expression.Term(Term number); Expression.Term role; Expression.Term concept ] ->
//                 match System.Int64.TryParse number with
//                 | true, number ->
//                     Ok(Expression.ConceptExpr(ConceptExpr.AtMost(role, ConceptExpr.AtomicConcept concept, number)))
//                 | _ -> error "Improper call to at-most." None
//             | [ Expression.Term(Term number); Expression.Term role; Expression.ConceptExpr concept ] ->
//                 match System.Int64.TryParse number with
//                 | true, number -> Ok(Expression.ConceptExpr(ConceptExpr.AtMost(role, concept, number)))
//                 | _ -> error "Improper call to at-most." None
//             | _ -> error "Improper call to at-most." None
//     )


let notFn: Fn =
    Fn(
        { doc = "Negate a Concept Expression."
          examples = [ "(not Dog)"; "(not (and Cat Dog))" ]
          args = "ConceptExpression"
          result = "ConceptExpression" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Expression.Term concept ] ->
                Ok(Expression.ConceptExpr(ConceptExpr.Not(ConceptExpr.AtomicConcept concept)))
            | [ Expression.ConceptExpr concept ] -> Ok(Expression.ConceptExpr(ConceptExpr.Not(concept)))
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
                            | Expression.Term term -> Ok(List.append state [ ConceptExpr.AtomicConcept term ])
                            | Expression.ConceptExpr expr -> Ok(List.append state [ expr ])
                            | _ -> error "Invalid argument." None
                        | _ -> state)
                    (Ok [])
                    arguments

            match res with
            | Ok value -> Ok(Expression.ConceptExpr(ConceptExpr.And value))
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
                            | Expression.Term term -> Ok(List.append state [ ConceptExpr.AtomicConcept term ])
                            | Expression.ConceptExpr expr -> Ok(List.append state [ expr ])
                            | _ -> error "Invalid argument." None
                        | _ -> state)
                    (Ok [])
                    arguments

            match res with
            | Ok value -> Ok(Expression.ConceptExpr(ConceptExpr.Or value))
            | Error err -> Error err
    )

let definitionsFn: Fn =
    Fn(
        { doc = "Create a set of definitions."
          examples = [ "(definitions (implies Dog Animal))" ]
          args = "Definition..."
          result = "Definitions" },
        fun _ _ _ arguments ->
            List.map
                (fun value ->
                    match value with
                    | Expression.Definition def -> def
                    | x -> failwith $"Not suported - {x}.")
                arguments
            |> Set.ofList
            |> Expression.Definitions
            |> Ok
    )
