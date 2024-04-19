// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.InMemory.Test

open Expecto
open Ligature.TestSuite

let callImpl _ _ _ _ = failwith "Not implemented"

[<Tests>]
let tests = ligatureTestSuite (fun () -> new LigatureInMemory(callImpl))

[<Tests>]
let wanderTests = wanderTestSuite (fun () -> new LigatureInMemory(callImpl))

[<Tests>]
let ligTests = ligTestSuite (fun () -> new LigatureInMemory(callImpl))

[<EntryPoint>]
let main argv = runTestsInAssemblyWithCLIArgs [] argv
