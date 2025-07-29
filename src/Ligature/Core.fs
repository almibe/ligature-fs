// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Core

open Ligature.Model

let individuals (aBox: Assertions) : Element list =
    Set.fold
        (fun state value ->
            match value with
            | Assertion.Instance(individual, _) -> //TODO check for nominal concept
                Set.add individual state
            | Assertion.Triple(i, _, f) -> Set.add i state |> Set.add f)
        Set.empty
        aBox
    |> List.ofSeq
