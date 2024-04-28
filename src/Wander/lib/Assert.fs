// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Assert

open Ligature.Wander.Model
open Ligature.Wander.InMemoryDataset
open Ligature
open Ligature.Wander.Pattern

let equalFunction<'t> =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.String(desc); left; right ] ->
                        if left = right then
                            Ok(WanderValue.Pattern(PatternSet(Set.empty)))
                        else
                            error $"{prettyPrint left} != {prettyPrint right}" None
                    | _ -> error "Invalid call to Assert.equal function." None)
            )
        )
    )

let failFunction<'t> =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(
                (fun args _ ->
                    match args with
                    | [ WanderValue.String(message) ] -> error message None
                    | _ -> error "Invalid call to Assert.fail function." None)
            )
        )
    )

let assertLib<'t> =
    WanderValue.Record(Map [ ("equal", equalFunction); ("fail", failFunction) ])
