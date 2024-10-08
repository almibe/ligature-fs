// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Interpreter

open Main
open Tokenizer
open NewParser
open Ligature.Main

// type Interpretation =
//     { Domain: Set<Element>
//       Concepts: Map<Element, Set<Element>>
//       Roles: Map<Element, Set<Element * Element>> }

// let interpret aBox : Result<Interpretation, TinyDLError> = failwith "TODO"

let parse (script: string) : Result<KnowledgeBase, TinyDLError> =
    match tokenize script with
    | Ok res ->
        match parse res with
        | Ok res -> express res
        | Error errorValue -> Error errorValue
    | Error errorValue -> Error errorValue
