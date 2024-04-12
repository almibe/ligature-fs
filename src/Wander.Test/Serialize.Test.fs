// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Bend.Main.Test

open Expecto
open Ligature.Bend.Model
open Ligature.Bend.Main
open Ligature

[<Tests>]
let tests =
    testList
        "Serialization Test"
        [ testCase "Simple write"
          <| fun _ ->
              
              let input =
                "\"hello\"\n\
                `a` `b` 123"
              let instance = new LigatureInMemory()
              let result = read 
              Expect.equal result (Ok(BendValue.Int(1235))) ""
        ]