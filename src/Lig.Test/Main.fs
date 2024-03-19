// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

open Expecto
open Ligature

// [<Tests>]
// let tests =
//   testList "Datasets Tests" [
//     testCase "Start with no Datasets" <| fun _ ->
//       let instance = new LigatureInMemory.LigatureInMemory() :> Ligature
//       let datasets = instance.AllDatasets()
//       Expect.equal (datasets) (Ok Array.empty) "Datasets should be empty."
//   ]

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly defaultConfig argv
