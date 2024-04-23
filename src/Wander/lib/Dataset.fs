// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Dataset

open Ligature.Wander.Model
open Ligature
open Ligature.Wander.InMemoryDataset

let countFunction<'t> =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(fun args _ -> failwith ""
                // match args with
                // | [ WanderValue.Array(statements) ] ->
                //     let statementSet = new System.Collections.Generic.HashSet<Statement>()

                //     Array.iter
                //         (fun statement ->
                //             match statement with
                //             | WanderValue.Statement(statement) -> statementSet.Add(statement) |> ignore
                //             | _ -> failwith "Unexpected value")
                //         statements

                //     Ok(WanderValue.Dataset(new InMemoryDataset(Set.ofSeq statementSet)))
                // | value -> error $"Unexpected value - {value}." None)
            )
        )
    )

let datasetLib<'t> = WanderValue.Record(Map [ ("count", countFunction) ])
