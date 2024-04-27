// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Pattern

open Ligature

type PatternSet(statements: Set<PatternStatement>) =
    interface IPattern with
        member _.Statements = statements
        member _.CompareTo(other) = failwith "TODO"
        member this.Apply: Map<Identifier,PatternValue> = 
            failwith "Not Implemented"
        member this.Extract(arg1: IPattern): Map<Identifier,PatternValue> = 
            failwith "Not Implemented"

let emptyPattern = PatternSet(Set.empty)