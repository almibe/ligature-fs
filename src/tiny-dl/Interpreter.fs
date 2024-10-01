// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Interpreter

//open TinyDL.Tokenizer
//open TinyDL.NewParser
open Ligature.Main

let acyclic (kb: Network) : bool = failwith "TODO"

// let normalize (: KnowledgeBase) : Result<ABox, TinyDLError> =
//     Set.fold
//         (fun state value ->
//             match value with
//             | Entry.Role bp -> failwith "TODO"
//             | Entry.Concept { element = symbol; concept = concept } ->
//                 match concept with
//                 | Symbol ac ->
//                     Set.add
//                         (Term.UnaryPredicate(
//                             { symbol = symbol
//                               concept = (NormalConceptExpression.AtomicConcept ac) }
//                         ))
//                         state
//                 // | Disjunction dj -> failwith "Not Implemented"
//                 // | Conjunction cj -> failwith "TODO"
//                 // match cj with
//                 // | { left = AtomicConcept left
//                 //     right = AtomicConcept right } -> failwith "TODO"
//                 // | _ -> failwith "TODO"
//                 // | Not { concept = AtomicConcept ac } ->
//                 //     Set.add
//                 //         (NormalABoxValue.UnaryPredicate(
//                 //             { symbol = symbol
//                 //               concept = (NormalConceptExpression.Not ac) }
//                 //         ))
//                 //         state
//                 // | Not(_) -> failwith "Not Implemented"
//             // | Definition(_) -> failwith "Not Implemented"
//             // | Inclusion(_) -> failwith "Not Implemented")
//         ) Set.empty
//         kb
//     |> Ok

type TinyDLError = string

let consistent (aBox: Network) : Result<bool, TinyDLError> =
    let mutable individuals: Map<Element, Set<Entry>> = Map.empty

    Set.fold
        (fun state (assertion: Entry) ->
            match state with
            | Error error -> Error error
            | Ok false -> Ok false
            | Ok true ->
                match assertion with
                | Concept { concept = conceptName
                            element = symbol } ->
                    let concept =
                        Concept
                            { concept = conceptName
                              element = symbol }

                    let notVersion =
                        NotConcept
                            { concept = conceptName
                              element = symbol }

                    match individuals.TryFind symbol with
                    | None ->
                        individuals <- Map.add symbol (Set.ofList [ concept ]) individuals
                        Ok(true)
                    | Some res ->
                        if res.Contains(notVersion) then
                            Ok(false)
                        else
                            individuals <- Map.add symbol (Set.add (concept) res) individuals
                            Ok(true)
                | NotConcept { concept = concept; element = symbol } ->
                    let notConcept = NotConcept { concept = concept; element = symbol }
                    let inverse = Concept { concept = concept; element = symbol }

                    match individuals.TryFind symbol with
                    | None ->
                        individuals <- Map.add symbol (Set.ofList [ notConcept ]) individuals
                        Ok(true)
                    | Some res ->
                        if res.Contains(inverse) then
                            Ok(false)
                        else
                            individuals <- Map.add symbol (Set.add notConcept res) individuals
                            Ok(true)
                | Role { first = first
                         second = second
                         role = role } -> Ok(true))
        (Ok(true))
        aBox

// let interpret (kb: KnowledgeBase) : Result<Interpretation, TinyDLError> =
//     let mutable domain = Set.empty
//     let mutable concepts = Map.empty
//     let mutable roles = Map.empty
//     let mutable definitions = Map.empty
//     let mutable inclusions = Map.empty

//     // Set.iter
//     //     (fun entry ->
//     //         match entry with
//     //         | Definition { left = left; right = right } ->
//     //             match concepts.TryFind left with
//     //             | None -> concepts <- Map.add (left) (Set.empty) concepts
//     //             | _ -> ()

//     //             // match concepts.TryFind right with
//     //             // | None -> concepts <- Map.add (right) (Set.empty) concepts
//     //             // | _ -> ()

//     //             match definitions.TryFind left with
//     //             | None -> definitions <- Map.add (left) (Set.ofList [ right ]) definitions
//     //             | Some res -> failwith "TODO"

//     //         // match definitions.TryFind right with
//     //         // | None -> definitions <- Map.add (right) (Set.ofList [ left ]) definitions
//     //         // | Some res -> failwith "TODO"

//     //         | Inclusion { left = left; right = right } ->
//     //             match concepts.TryFind left with
//     //             | None -> concepts <- Map.add (left) (Set.empty) concepts
//     //             | _ -> ()

//     //             // match concepts.TryFind right with
//     //             // | None -> concepts <- Map.add (right) (Set.empty) concepts
//     //             // | _ -> ()

//     //             match inclusions.TryFind left with
//     //             | None -> inclusions <- Map.add (left) (Set.ofList [ right ]) inclusions
//     //             | Some res -> failwith "TODO")
//     //     tBox

//     // Set.iter
//     //     (fun entry ->
//     //         match entry with
//     //         | UnaryPredicate up ->
//     //             match up with
//     //             | { symbol = symbol; concept = concept } ->
//     //                 // match concepts.TryFind concept with
//     //                 // | Some(res) -> concepts <- Map.add (concept) (Set.add symbol res) concepts
//     //                 // | None -> concepts <- Map.add (concept) (Set.ofList [ symbol ]) concepts

//     //                 domain <- Set.add (symbol) domain

//     //             // match definitions.TryFind concept with
//     //             // | Some res -> failwith "TODO"
//     //             //     // Set.iter
//     //             //     //     (fun concept ->
//     //             //     //         match concepts.TryFind concept with
//     //             //     //         | Some(res) -> concepts <- Map.add (concept) (Set.add symbol res) concepts
//     //             //     //         | None -> concepts <- Map.add (concept) (Set.ofList [ symbol ]) concepts)
//     //             //     //     res
//     //             // | _ -> ()

//     //             // match inclusions.TryFind concept with
//     //             // | Some res -> failwith "TODO"
//     //             //     // Set.iter
//     //             //     //     (fun concept ->
//     //             //     //         match concepts.TryFind concept with
//     //             //     //         | Some(res) -> concepts <- Map.add (concept) (Set.add symbol res) concepts
//     //             //     //         | None -> concepts <- Map.add (concept) (Set.ofList [ symbol ]) concepts)
//     //             //     //     res
//     //             // | _ -> ()

//     //             | _ -> failwith "TODO"
//     //         | BinaryPredicate bp ->
//     //             domain <- Set.add (bp.left) domain
//     //             domain <- Set.add (bp.right) domain
//     //             roles <- Map.add bp.role (Set.ofList [ bp.left, bp.right ]) roles)
//     //     aBox

//     Ok
//         { Domain = domain
//           Concepts = concepts
//           Roles = roles }

// let eval (script: string) : Result<Interpretation, TinyDLError> =
//     match tokenize script with
//     | Ok res ->
//         match parse res with
//         | Ok res -> failwith "TODO" //interpret res
//         | Error errorValue -> Error errorValue
//     | Error errorValue -> Error errorValue
