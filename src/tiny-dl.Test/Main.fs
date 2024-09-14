// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Test

open Expecto
//open TinyDL.Main

// [<Tests>]
// let tests =
//     testList
//         "Tests"
//         [ testCase "Check Triple Equality"
//           <| fun _ ->
//               Expect.equal
//                   (((Symbol("a"))), ((Symbol("b"))), ((Symbol("c"))))
//                   (((Symbol("a"))), ((Symbol("b"))), ((Symbol("c"))))
//                   "" ]

[<EntryPoint>]
let main argv = runTestsInAssemblyWithCLIArgs [] argv
