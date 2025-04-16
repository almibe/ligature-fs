// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Model

open Ligature.Model

[<RequireQualifiedAccess>]
type ConceptExpr =
    | AtomicConcept of Term
    | Conjunction of ConceptExpr list
    | Top
    | Bottom
    | Exists of Term * ConceptExpr
    | All of Term * ConceptExpr

[<RequireQualifiedAccess>]
type Definition =
    | Implies of Term * ConceptExpr
    | Define of Term * ConceptExpr

type Definitions = Set<Definition>

let printDefinition definition = "def"

let printDefinitions definition = "defs"
