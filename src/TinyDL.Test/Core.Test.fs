// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Test.Core

open Expecto
open Ligature.Model
open Ligature.Core
open TinyDL.Core

[<Tests>]
let tests =
    testList
        "Infer Test Suite"
        [
          testCase "empty call to infer"
            <| fun _ ->
                let result = infer List.empty Set.empty
                Expect.equal result Set.empty ""
          ]
