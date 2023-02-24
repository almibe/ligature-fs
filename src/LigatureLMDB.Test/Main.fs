// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.LMDB.Test

open Ligature.LMDB.Main
open Expecto
open Ligature.TestSuite

[<Tests>]
let tests = ligatureTestSuite (fun () ->
    let instance = new LigatureLMDB()
    instance.initialize () |> ignore
    instance)

[<EntryPoint>]
let main argv =
    runTestsInAssembly defaultConfig argv
