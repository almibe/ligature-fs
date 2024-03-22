// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Bend.Lib.Preludes
open Ligature.Bend
open Bool

let bindStandardLibrary bindings =
    bindings
    |> Bindings.bind "Bool" boolLib

// let bindInstanceLevelFunctions instance bindings =
//     bindings
//     |> Bindings.bind "datasets" (Instance.datasetsFunction instance)
//     |> Bindings.bind "createDataset" (Instance.addDataset instance)
//     |> Bindings.bind "removeDataset" (Instance.removeDataset instance)
//     |> Bindings.bind "datasetExists" (Instance.datasetExists instance)
//     |> Bindings.bind "query" (Instance.query instance)
//     |> Bindings.bind "write" (Instance.write instance)
//     |> Bindings.bind "allStatements" (Instance.allStatements instance)
//     |> Bindings.bind "match" (Instance.matchCommand instance)

let standardPrelude () =
    bindStandardLibrary (Bindings.newBindings ())

// let instancePrelude (instance: ILigature): Bindings = 
//     bindStandardLibrary (Bindings.newBindings ())
//     |> bindInstanceLevelFunctions instance
