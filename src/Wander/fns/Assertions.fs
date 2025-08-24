// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Fns.Assertions

open Ligature.Model
open Wander.Model
open Ligature.Interpreter

let assertionsFn =
    Fn.Fn(
        { doc = "Create a set of assertions."
          examples = [ "assertions(instance(betty Cat))" ]
          args = "Assertion..."
          result = "Assertions" },
        fun _ _ application ->
            let mutable res: Assertions = Set.empty

            List.iter
                (fun arg ->
                    match arg with
                    | Expression.Assertions assertions -> res <- Set.union assertions res
                    | Expression.Assertion assertion -> res <- Set.add assertion res
                    | Expression.Element element -> res <- Set.add (Assertion.Instance(element, ConceptExpr.Top)) res
                    | x -> failwith $"Invalid call to assertions: {x}")
                application.arguments

            Ok(Expression.Assertions res)
    )

let patternFn =
    Fn.Fn(
        { doc = "Create a pattern."
          examples = [ "pattern(instance(?cat Cat))" ]
          args = "Patterns..."
          result = "Pattern" },
        fun _ _ application ->
            let mutable res: Pattern = Set.empty

            // List.iter
            //     (fun arg ->
            //         match arg with
            //         | Expression.Assertions assertions -> res <- Set.union assertions res
            //         | Expression.Assertion assertion -> res <- Set.add assertion res
            //         | Expression.Element element -> res <- Set.add (Assertion.Instance(element, ConceptExpr.Top)) res
            //         | x -> failwith $"Invalid call to assertions: {x}")
            //     application.arguments

            Ok(Expression.Pattern res)
    )

let resultFn =
    Fn.Fn(
        { doc = "Create a result."
          examples = [ "result(?a -> test)" ]
          args = "Slot -> Value..."
          result = "Result" },
        fun _ _ application ->
            let mutable res = Map.empty

            let mutable args = application.arguments
            let mutable cont = true

            while cont do
                match args with
                | [] -> cont <- false
                | Expression.Slot key :: Expression.Term(Term "->") :: Expression.Element value :: tail ->
                    res <- Map.add key value res
                    args <- tail
                | Expression.Slot key :: Expression.Term(Term "->") :: Expression.Term value :: tail ->
                    res <- Map.add key (termToElement value) res
                    args <- tail
                | _ -> failwith "TODO"

            // List.iter
            //     (fun arg ->
            //         match arg with
            //         | Expression.Assertions assertions -> res <- Set.union assertions res
            //         | Expression.Assertion assertion -> res <- Set.add assertion res
            //         | Expression.Element element -> res <- Set.add (Assertion.Instance(element, ConceptExpr.Top)) res
            //         | x -> failwith $"Invalid call to assertions: {x}")
            //     application.arguments

            Ok(Expression.Result res)
    )


let resultSetFn =
    Fn.Fn(
        { doc = "Create a result set."
          examples = [ "result-set()" ]
          args = "Results..."
          result = "ResultSet" },
        fun _ _ application ->
            let mutable res = Set.empty

            List.iter
                (fun arg ->
                    match arg with
                    | Expression.Result r -> res <- Set.add r res
                    | x -> failwith $"Invalid call to result-set: {x}")
                application.arguments

            Ok(Expression.ResultSet res)
    )

let unionFn =
    Fn.Fn(
        { doc = "Combine the top two Networks on the Stack and push the resulting Network."
          examples = []
          args = "Assertions Assertions"
          result = "Assertions" },
        fun _ _ arguments -> failwith "TODO"
    // match arguments with
    // | [ Expression.Assertions left; Expression.Assertions right ] ->
    //     let result = Set.union left right |> Expression.Assertions
    //     Ok result
    // | _ -> failwith $"Calls to union requires two ABoxes."
    )

let countFn =
    Fn.Fn(
        { doc = "Count the number of items in a seq."
          examples = [ "count(assertions(triple(a b c)))" ]
          args = "Assertions"
          result = "Term" },
        fun _ _ application ->
            match application.arguments with
            | [ Expression.Assertions n ] ->
                Ok(
                    Expression.Element
                        { value = Term((Set.count n).ToString())
                          space = None
                          langTag = None }
                )
            | [ Expression.Seq seq ] ->
                Ok(
                    Expression.Element
                        { value = Term((List.length seq).ToString())
                          space = None
                          langTag = None }
                )
            | _ -> error "Illegal call to count." None
    )

let aBoxToNode (individual: Term) (aBox: Assertions) : Application = //TODO also accept a TBox and Concept to control
    { name = individual
      attributes = Map.empty
      arguments = [] }
// let selectionValues =
//     List.fold
//         (fun state value ->
//             match value with
//             | Expression.Term roleName ->
//                 List.ofSeq aBox
//                 |> List.fold
//                     (fun state value ->
//                         match value with
//                         // | Assertion.Triple(i, r, v) when i = individual && r = roleName ->
//                         //     match v with
//                         //     | Value.Literal l -> Map.add r (Expression.Literal l) state
//                         //     | Value.Term t -> Map.add r (Expression.Term t) state
//                         | _ -> failwith "TODO") //state)
//                     state
//             | _ -> failwith "TODO")
//         Map.empty
//         selections

// { name = individual
//   attributes = selectionValues
//   children = [] }

let queryFn =
    Fn.Fn(
        { doc = "Perform a query."
          examples = []
          args = "Definitions Assertions Pattern"
          result = "ResultSet" },
        fun _ _ application ->
            match application.arguments with
            | [ Expression.Definitions tBox; Expression.Assertions aBox; Expression.Pattern pattern ] ->
                let results = query tBox aBox pattern
                Ok(Expression.ResultSet results)
            | _ -> error "Invalid call to query" None
    )

let instancesFn =
    Fn.Fn(
        { doc = "Find instances of a Concept."
          examples = []
          args = "Definitions Assertions (Term | ConceptExpr)"
          result = "Seq" },
        fun _ _ application ->
            match application.arguments with
            | [ Expression.Definitions tBox; Expression.Assertions aBox; Expression.Term concept ] ->
                let results =
                    instances tBox aBox (ConceptExpr.AtomicConcept concept)
                    |> List.map (fun value -> Expression.Element value)
                //aBoxToNode (Term i) aBox |> Expression.NodeLiteral)

                Ok(Expression.Seq results)
            | [ Expression.Definitions tBox; Expression.Assertions aBox; Expression.ConceptExpr concept ] ->
                let results =
                    instances tBox aBox concept |> List.map (fun value -> Expression.Element value)
                // aBoxToNode (Term i) aBox |> Expression.NodeLiteral)

                Ok(Expression.Seq results)
            | _ -> error "Invalid call to instances" None
    )

// let applyCommand =
//     { Eval =
//         fun networks local modules variables arguments ->
//             match arguments with
//             | [ Any.Network network; Any.Tuple q ] ->
//                 match evalTuple networks local modules variables q with
//                 | Ok((Some(Any.ResultSet res), networks, local, modules, variables)) ->
//                     let res = apply network res
//                     Ok((Some(Any.Network res), networks, local, modules, variables))
//                 | Ok((Some(Any.ValueSet res), networks, local, modules, variables)) ->
//                     let res = applyValueSet network res
//                     Ok((Some(Any.Network res), networks, local, modules, variables))
//                 | Ok _ -> failwith "TODO"
//                 | Error err -> error $"Error in apply. {err.UserMessage}" None
//             | [ Any.Network network; Any.Slot v ] ->
//                 match Map.tryFind v variables with
//                 | Some(Any.ResultSet res) ->
//                     let res = apply network res
//                     Ok((Some(Any.Network res), networks, local, modules, variables))
//                 | Some(Any.ValueSet res) ->
//                     let res = applyValueSet network res
//                     Ok((Some(Any.Network res), networks, local, modules, variables))
//                 | Some _ -> failwith "TODO"
//                 | None -> failwith "TODO"
//             | args -> failwith $"TODO - unexpected args {args}" }


// let filterFn =
//     Fn.Fn(
//         { doc = "Accepts two Networks. First a Pattern and then a Network to search. Pushes the matching Network."
//           examples = []
//           args = "Pattern Network"
//           result = "Network" },
//         fun _ _ _ arguments ->
//             match arguments with
//             | [ Any.Pattern pattern; Any.ABox source ] ->
//                 // let pattern =
//                 //     match pattern with
//                 //     | Any.Network n -> n
//                 //     | Any.Slot v ->
//                 //         if variables.ContainsKey v then
//                 //             match variables[v] with
//                 //             | Any.Network n -> n
//                 //             | _ -> failwith "TODO"
//                 //         else
//                 //             failwith "TODO"
//                 //     | Any.Tuple tuple ->
//                 //         match evalTuple networks local modules variables tuple with
//                 //         | Ok((Some(Any.Network n), networks, local, modules)) -> n
//                 //         | _ -> failwith "TODO"
//                 //     | _ -> failwith "TODO"

//                 // let source =
//                 //     match source with
//                 //     | Any.Network n -> n
//                 //     | Any.Slot v ->
//                 //         if variables.ContainsKey v then
//                 //             match variables[v] with
//                 //             | Any.Network n -> n
//                 //             | _ -> failwith "TODO"
//                 //         else
//                 //             failwith "TODO"
//                 //     | Any.Tuple tuple ->
//                 //         match evalTuple networks local modules variables tuple with
//                 //         | Ok((Some(Any.Network n), networks, local, modules)) -> n
//                 //         | _ -> failwith "TODO"
//                 //     | _ -> failwith "TODO"

//                 let results = filter pattern source
//                 Ok(Any.ABox results)
//             | _ -> error "Invalid call to filter" None
//     )

let isEmptyFn =
    Fn.Fn(
        { doc =
            "Takes a Network or Tuple off the top of the Stack and pushes \"true\" if it is empty or \"false\" if not."
          examples = []
          args = ""
          result = "" },
        fun actions variables arguments -> failwith "TODO"
    )
