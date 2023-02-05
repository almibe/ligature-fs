// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.InMemory.Test

open Expecto
open Ligature.TestSuite

[<Tests>]
let tests = ligatureTestSuite ()

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly defaultConfig argv
