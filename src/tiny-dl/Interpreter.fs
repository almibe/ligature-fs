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

    Set.iter
        (fun entry ->
            match entry with
            | UnaryPredicate up ->
                match up with
                | { symbol = symbol
                    concept = AtomicConcept(concept) } ->
                    //TODO check if concept already exists
                    concepts <- Map.add (concept) (Set.ofList [ symbol ]) concepts
                    domain <- Set.add (symbol) domain
                    domain <- Set.add (concept) domain
                | _ -> failwith "TODO"
            | BinaryPredicate bp -> failwith "TODO")
        aBox

    Set.iter
        (fun entry ->
            match entry with
            | AtomicConcept c -> failwith "TODO"
            | Disjunction d -> failwith "Not Implemented"
            | Conjunction c -> failwith "Not Implemented"
            | Equivalence { left = AtomicConcept left
                            right = AtomicConcept right } ->
                //TODO check if concept already exists
                concepts <- Map.add (left) (Set.empty) concepts
                concepts <- Map.add (right) (Set.empty) concepts
            | Not n -> failwith "Not Implemented"
            | Subsumption s -> failwith "Not Implemented"
            | Equivalence(_) -> failwith "Not Implemented")
        tBox

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
