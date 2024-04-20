// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.InMemory.Test

open Expecto
open Ligature.TestSuite

[<Tests>]
let tests = ligatureTestSuite (fun () -> new LigatureInMemory())

[<Tests>]
let wanderTests = wanderTestSuite (fun () -> new LigatureInMemory())

[<Tests>]
let ligTests = ligTestSuite (fun () -> new LigatureInMemory())

[<EntryPoint>]
let main argv = runTestsInAssemblyWithCLIArgs [] argv
