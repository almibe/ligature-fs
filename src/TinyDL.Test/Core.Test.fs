// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Core.Test

open Expecto
open TinyDL.Model
open TinyDL.Core

[<Tests>]
let tests =
    testList
        "Core Test Suite"
        [ testCase "empty infer"
          <| fun _ ->
              let result = infer Set.empty Set.empty
              Expect.equal result Set.empty "" ]
