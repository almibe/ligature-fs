// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Model

open Ligature.Model

type ConceptExpr = 
    | AtomicConcept of Term
    | Conjunction of ConceptExpr list

[<RequireQualifiedAccess>]
type Definition =
    | Subconcept of Term * ConceptExpr
    | Equivalent of Term * ConceptExpr

type Definitions = Set<Definition>

let printDefinition definition = "def"

let printDefinitions definition = "defs"
