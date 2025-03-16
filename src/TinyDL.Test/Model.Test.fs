// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Test.Model

open Expecto
open Ligature.Model
open Ligature.Core
open TinyDL.Model

[<Tests>]
let tests =
    testList
        "Network to Model Test Suite"
        [ testCase "pass empty network"
          <| fun _ ->
              let result = networkToModel Set.empty
              Expect.equal result Set.empty "" ]
