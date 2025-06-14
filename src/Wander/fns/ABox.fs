// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Fns.Network

open Ligature.Model
open Wander.Model
open Ligature.Core
open Ligature.Interpreter
open Wander.Interpreter

let rec nodeToNetwork (node: Node) : Result<ABox, LigatureError> = failwith "TODO"
// match Map.tryFind (Any.Term(Term "@")) record with
// | Some(Any.Term id) ->
//     Seq.fold
//         (fun state (key, value) ->
//             if key = Any.Term(Term "@") then
//                 state
//             else
//                 let role: Term =
//                     match key with
//                     | Any.Term term -> term
//                     | _ -> failwith "TODO"

//                 match value with
//                 | Any.Literal literal -> Set.add (Assertion.Triple(id, role, Value.Literal literal)) state
//                 | Any.Term term ->
//                     if role = Term ":" then
//                         Set.add (Assertion.Instance(id, ConceptExpr.AtomicConcept term)) state
//                     else if role = Term "~" then
//                         Set.add (Assertion.Instance(id, ConceptExpr.Not(ConceptExpr.AtomicConcept term))) state
//                     else
//                         Set.add (Assertion.Triple(id, role, Value.Term term)) state
//                 | Any.Tuple tuple ->
//                     List.fold
//                         (fun state value ->
//                             match value with
//                             | Any.Literal literal ->
//                                 Set.add (Assertion.Triple(id, role, Value.Literal literal)) state
//                             | Any.Term term ->
//                                 if role = Term ":" then
//                                     Set.add (Assertion.Instance(id, ConceptExpr.AtomicConcept term)) state
//                                 else if role = Term "~" then
//                                     failwith "TODO"
//                                 else
//                                     Set.add (Assertion.Triple(id, role, Value.Term term)) state
//                             | _ -> failwith "TODO")
//                         state
//                         tuple
//                 | Any.Node record ->
//                     let state =
//                         match record.TryFind(Any.Term(Term "@")) with
//                         | Some(Any.Term value) -> Set.add (Assertion.Triple(id, role, Value.Term value)) state
//                         | _ -> failwith "TODO"

//                     match recordToNetwork record with
//                     | Ok network -> state + network
//                     | _ -> failwith "TODO"
//                 | _ -> failwith "TODO")
//         Set.empty
//         (Map.toSeq record)
//     |> Ok
// | _ -> error "Record requires valid @ entry." None

//let rec nodeToPattern (node: Node) : Result<Pattern, LigatureError> = failwith "TODO"
// match Map.tryFind (Any.Term(Term "@")) record with
// | Some id ->
//     let id =
//         match id with
//         | Any.Term term -> TermPattern.Term term
//         | Any.Slot slot -> TermPattern.Slot slot
//         | _ -> failwith "TODO"

//     Seq.fold
//         (fun state (key, value) ->
//             if key = Any.Term(Term "@") then
//                 state
//             else
//                 let role: TermPattern =
//                     match key with
//                     | Any.Term term -> TermPattern.Term term
//                     | Any.Slot slot -> TermPattern.Slot slot
//                     | _ -> failwith "TODO"

//                 match value with
//                 | Any.Literal literal -> Set.add (id, role, ValuePattern.Literal literal) state
//                 | Any.Term term -> Set.add (id, role, ValuePattern.Term term) state
//                 | Any.Slot slot -> Set.add (id, role, ValuePattern.Slot slot) state
//                 | Any.Tuple tuple ->
//                     List.fold
//                         (fun state value ->
//                             match value with
//                             | Any.Literal literal -> Set.add (id, role, ValuePattern.Literal literal) state
//                             | Any.Term term -> Set.add (id, role, ValuePattern.Term term) state
//                             | Any.Slot slot -> Set.add (id, role, ValuePattern.Slot slot) state
//                             | _ -> failwith "TODO")
//                         state
//                         tuple
//                 | Any.Record record ->
//                     let state =
//                         match record.TryFind(Any.Term(Term "@")) with
//                         | Some(Any.Term value) -> Set.add (id, role, ValuePattern.Term value) state
//                         | Some(Any.Slot slot) -> Set.add (id, role, ValuePattern.Slot slot) state
//                         | _ -> failwith "TODO"

//                     match recordToPattern record with
//                     | Ok pattern -> state + pattern
//                     | _ -> failwith "TODO"
//                 | _ -> failwith "TODO")
//         Set.empty
//         (Map.toSeq record)
//     |> Ok
// | _ -> error "Record requires valid @ entry." None

let aBoxFn =
    Fn(
        { doc = "Create an ABox."
          examples = [ "(a-box (instance betty Cat))" ]
          args = "Assertion..."
          result = "Assertions" },
        fun _ _ _ arguments ->
            let mutable res: ABox = Set.empty

            List.iter
                (fun arg ->
                    match arg with
                    | Expression.Assertion assertion -> res <- Set.add assertion res
                    | Expression.Tuple [ e; a; v ] ->
                        let e =
                            match e with
                            | Expression.Term t -> t
                            | _ -> failwith "Invalid call to a-box."

                        let a =
                            match a with
                            | Expression.Term t -> t
                            | _ -> failwith "Invalid call to a-box."

                        let v =
                            match v with
                            | Expression.Term t -> failwith "TODO" //Value.Term t
                            | Expression.Literal l -> failwith "TODO" //Value.Literal l
                            | _ -> failwith "Invalid call to a-box."

                        failwith "TODO"
                    // match a, v with
                    // | Term ":", Value.Term concept ->
                    //     res <- Set.add (Assertion.Instance(e, ConceptExpr.AtomicConcept concept)) res
                    // | Term "~", Value.Term concept ->
                    //     res <-
                    //         Set.add (Assertion.Instance(e, ConceptExpr.Not(ConceptExpr.AtomicConcept concept))) res
                    // | _ -> res <- Set.add (Assertion.Triple(e, a, v)) res
                    | Expression.NodeLiteral record ->
                        match nodeToNetwork record with
                        | Ok network -> res <- res + network
                        | _ -> failwith "TODO"
                    | _ -> failwith "Invalid call to a-box.")
                arguments

            Ok(Expression.ABox res)
    )

let unionFn =
    Fn(
        { doc = "Combine the top two Networks on the Stack and push the resulting Network."
          examples = [ "{a b c} {d e f} union\n{a b c, d e f} assert-equal" ]
          args = "Network Network"
          result = "Network" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Expression.ABox left; Expression.ABox right ] ->
                let result = Set.union left right |> Expression.ABox
                Ok result
            | _ -> failwith $"Calls to union requires two ABoxes."
    )

let countFn =
    Fn(
        { doc = "Count the assertions in a given ABox."
          examples = [ "(count (assertions [a b c]))" ]
          args = "Assertions"
          result = "Literal" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Expression.ABox n ] ->
                Ok(
                    Expression.Literal
                        { value = (Set.count n).ToString()
                          typeof = None
                          langTag = None }
                )
            | _ -> error "Illegal call to count." None
    )

// let minusCommand =
//     { Eval =
//         fun networks local modules (arguments: Arguments) ->
//             match arguments with
//             | [ Any.Network(left); Any.Network(right) ] ->
//                 let result = Set.difference left right |> Any.Network
//                 Ok(networks, local, modules)
//             | _ -> failwith "TODO" }

let aBoxToNode (individual: Term) (aBox: ABox) (selections: Expression list) : Node = //TODO also accept a TBox and Concept to control
    let selectionValues =
        List.fold
            (fun state value ->
                match value with
                | Expression.Term roleName ->
                    List.ofSeq aBox
                    |> List.fold
                        (fun state value ->
                            match value with
                            // | Assertion.Triple(i, r, v) when i = individual && r = roleName ->
                            //     match v with
                            //     | Value.Literal l -> Map.add r (Expression.Literal l) state
                            //     | Value.Term t -> Map.add r (Expression.Term t) state
                            | _ -> failwith "TODO") //state)
                        state
                | _ -> failwith "TODO")
            Map.empty
            selections

    { name = individual
      attributes = selectionValues
      children = [] }

let queryFn =
    Fn(
        { doc = "Perform a query."
          examples = []
          args = "TBox ABox (Term | ConceptExpr) Tuple"
          result = "Tuple" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Expression.TBox tBox; Expression.ABox aBox; Expression.Term concept; Expression.Tuple selections ] ->
                failwith "TODO"
            // let results =
            //     query tBox aBox (ConceptExpr.AtomicConcept concept)
            //     |> List.map (fun (i, a) -> aBoxToNode i aBox selections |> Expression.NodeLiteral)

            // Ok(Expression.Tuple results)
            | [ Expression.TBox tBox; Expression.ABox aBox; Expression.ConceptExpr concept; Expression.Tuple selections ] ->
                failwith "TODO"
            // let results =
            //     query tBox aBox concept
            //     |> List.map (fun (i, a) -> aBoxToNode i aBox selections |> Expression.NodeLiteral)

            // Ok(Expression.Tuple results)
            | _ -> error "Invalid call to query" None
    )

// let matchCommand =
//     { Eval =
//         fun networks local modules arguments ->
//             match arguments with
//             | [ Any.Tuple [ e; a; v ]; Any.Network network ] ->
//                 let element =
//                     match e with
//                     | Any.Term e -> TermPattern.Term e
//                     | Any.Slot v -> TermPattern.Slot v
//                     | _ -> failwith "TODO"

//                 let attribute =
//                     match a with
//                     | Any.Term e -> TermPattern.Term e
//                     | Any.Slot v -> TermPattern.Slot v
//                     | _ -> failwith "TODO"

//                 let value =
//                     match v with
//                     | Any.Term e -> Value.Term e
//                     | Any.Slot v -> Value.Slot v
//                     | Any.Literal l -> Value.Literal l
//                     | _ -> failwith "TODO"

//                 Ok(
//                     (Some(Any.ResultSet(singleMatch (element, attribute, value) network)),
//                      networks,
//                      local,
//                      modules,
//                      variables)
//                 )
//             | [ pattern; network ] ->
//                 let pattern =
//                     match pattern with
//                     | Any.Network n -> n
//                     | Any.Tuple q ->
//                         match evalTuple networks local modules variables q with
//                         | Ok((Some(Any.Network n), networks, local, modules, variables)) -> n
//                         | _ -> failwith "TODO"
//                     | _ -> failwith "TODO"

//                 let network =
//                     match network with
//                     | Any.Network n -> n
//                     | Any.Tuple q ->
//                         match evalTuple networks local modules variables q with
//                         | Ok((Some(Any.Network n), networks, local, modules, variables)) -> n
//                         | _ -> failwith "TODO"
//                     | _ -> failwith "TODO"

//                 Ok(Some(Any.ResultSet(networkMatch pattern network)), networks, local, modules, variables)

//             | _ -> failwith "TODO" }

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
//     Fn(
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
    Fn(
        { doc =
            "Takes a Network or Tuple off the top of the Stack and pushes \"true\" if it is empty or \"false\" if not."
          examples = []
          args = ""
          result = "" },
        fun actions variables arguments -> failwith "TODO"
    // match stack with
    // | Any.Network cond :: tail ->
    //     if cond = Set.empty then
    //         Ok(Any.Term(Term "true") :: tail)
    //     else
    //         Ok(Any.Term(Term "false") :: tail)
    // | Any.Tuple q :: tail ->
    //     if q.IsEmpty then
    //         Ok(Any.Term(Term "true") :: tail)
    //     else
    //         Ok(Any.Term(Term "false") :: tail)
    // | _ -> error "Invalid call to is-empty" None
    )
