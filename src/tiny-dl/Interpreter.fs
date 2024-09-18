// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Interpreter

open TinyDL.Main
open TinyDL.Tokenizer
open TinyDL.Parser

let interpret ((tBox, aBox): KnowledgeBase) : Result<Interpretation, TinyDLError> =
    let mutable domain = Set.empty
    let mutable concepts = Map.empty
    let mutable roles = Map.empty
    let mutable definitions = Map.empty
    let mutable inclusions = Map.empty

    Set.iter
        (fun entry ->
            match entry with
            | AtomicConcept c -> failwith "TODO"
            | Disjunction d -> failwith "Not Implemented"
            | Conjunction c -> failwith "Not Implemented"
            | Definition { left = left
                           right = AtomicConcept right } ->
                match concepts.TryFind left with
                | None -> concepts <- Map.add (left) (Set.empty) concepts
                | _ -> ()

                match concepts.TryFind right with
                | None -> concepts <- Map.add (right) (Set.empty) concepts
                | _ -> ()

                match definitions.TryFind left with
                | None -> definitions <- Map.add (left) (Set.ofList [ right ]) definitions
                | Some res -> failwith "TODO"

                match definitions.TryFind right with
                | None -> definitions <- Map.add (right) (Set.ofList [ left ]) definitions
                | Some res -> failwith "TODO"

            | Not n -> failwith "Not Implemented"
            | Inclusion { left = left
                          right = AtomicConcept right } ->
                match concepts.TryFind left with
                | None -> concepts <- Map.add (left) (Set.empty) concepts
                | _ -> ()

                match concepts.TryFind right with
                | None -> concepts <- Map.add (right) (Set.empty) concepts
                | _ -> ()

                match inclusions.TryFind left with
                | None -> inclusions <- Map.add (left) (Set.ofList [ right ]) inclusions
                | Some res -> failwith "TODO"
            | Definition(_) -> failwith "Not Implemented")
        tBox

    Set.iter
        (fun entry ->
            match entry with
            | UnaryPredicate up ->
                match up with
                | { symbol = symbol; concept = concept } ->
                    match concepts.TryFind concept with
                    | Some(res) -> concepts <- Map.add (concept) (Set.add symbol res) concepts
                    | None -> concepts <- Map.add (concept) (Set.ofList [ symbol ]) concepts

                    domain <- Set.add (symbol) domain

                    match definitions.TryFind concept with
                    | Some res ->
                        Set.iter
                            (fun concept ->
                                match concepts.TryFind concept with
                                | Some(res) -> concepts <- Map.add (concept) (Set.add symbol res) concepts
                                | None -> concepts <- Map.add (concept) (Set.ofList [ symbol ]) concepts)
                            res
                    | _ -> ()

                    match inclusions.TryFind concept with
                    | Some res ->
                        Set.iter
                            (fun concept ->
                                match concepts.TryFind concept with
                                | Some(res) -> concepts <- Map.add (concept) (Set.add symbol res) concepts
                                | None -> concepts <- Map.add (concept) (Set.ofList [ symbol ]) concepts)
                            res
                    | _ -> ()

                | _ -> failwith "TODO"
            | BinaryPredicate bp ->
                domain <- Set.add (bp.left) domain
                domain <- Set.add (bp.right) domain
                roles <- Map.add bp.role (Set.ofList [ bp.left, bp.right ]) roles)
        aBox

    Ok
        { Domain = domain
          Concepts = concepts
          Roles = roles }

let eval (script: string) : Result<Interpretation, TinyDLError> =
    match tokenize script with
    | Ok res ->
        match parse res with
        | Ok res -> interpret res
        | Error errorValue -> Error errorValue
    | Error errorValue -> Error errorValue
