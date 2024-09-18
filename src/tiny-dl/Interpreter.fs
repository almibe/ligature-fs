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
    let mutable equivs = Map.empty

    Set.iter
        (fun entry ->
            match entry with
            | AtomicConcept c -> failwith "TODO"
            | Disjunction d -> failwith "Not Implemented"
            | Conjunction c -> failwith "Not Implemented"
            | Equivalence { left = AtomicConcept left
                            right = AtomicConcept right } ->
                match concepts.TryFind left with
                | None -> concepts <- Map.add (left) (Set.empty) concepts
                | _ -> ()

                match concepts.TryFind right with
                | None -> concepts <- Map.add (right) (Set.empty) concepts
                | _ -> ()

                domain <- Set.add (left) domain
                domain <- Set.add (right) domain

                match equivs.TryFind left with
                | None -> equivs <- Map.add (left) (Set.ofList [ right ]) equivs
                | Some res -> failwith "TODO"

                match equivs.TryFind right with
                | None -> equivs <- Map.add (right) (Set.ofList [ left ]) equivs
                | Some res -> failwith "TODO"

            | Not n -> failwith "Not Implemented"
            | Subsumption s -> failwith "Not Implemented"
            | Equivalence(_) -> failwith "Not Implemented")
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
                    domain <- Set.add (concept) domain

                    match equivs.TryFind concept with
                    | Some res ->
                        Set.iter
                            (fun concept ->
                                match concepts.TryFind concept with
                                | Some(res) -> concepts <- Map.add (concept) (Set.add symbol res) concepts
                                | None -> concepts <- Map.add (concept) (Set.ofList [ symbol ]) concepts)
                            res
                    | _ -> ()

                | _ -> failwith "TODO"
            | BinaryPredicate bp -> failwith "TODO")
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
        | Error(errorValue) -> failwith "Not Implemented"
    | Error(errorValue) -> Error(TinyDLError "Error tokenizing.")
