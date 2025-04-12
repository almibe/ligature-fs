// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Model

open Ligature.Model

type Concept = AtomicConcept of Term

type Definition =
    | Subconcept of Concept * Concept
    | Equivalent of Concept * Concept

let infer (tBox: Set<Definition>) (aBox: Network) =
    aBox
