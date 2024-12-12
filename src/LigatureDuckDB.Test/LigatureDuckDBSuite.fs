// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.DuckDB.Test.Suite

open Expecto
open Ligature.Main
open Ligature.DuckDB

[<Tests>]
let tests =
    testList
        "DuckDB Store"
        [ testCase "Start with no networks."
          <| fun _ ->
              let store = inMemoryDuckDBStore ()
              Expect.equal (store.Networks()) (Ok(Set.empty)) "" ]