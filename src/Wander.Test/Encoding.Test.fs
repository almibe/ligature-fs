// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Bend.Test.Encoding

open Expecto
open Ligature.Bend.Main
open Ligature.Bend.Lib.Preludes
open Ligature.Bend.Model

let config = { FsCheckConfig.defaultConfig with maxTest = 10000 }

[<Tests>]
let properties =
  testList "Encoding and Decoding Tests" [
    testProperty "Encode and decode Int" <|
      fun (value: int64) ->
        match run (prettyPrint (BendValue.Int(value))) (standardPrelude ()) with
        | Ok(BendValue.Int(res)) -> value = res
        | _ -> false
    testProperty "Encode and decode String" <|
      fun (value: string) ->
        if value <> null then
          match run (prettyPrint (BendValue.String(value))) (standardPrelude ()) with
          | Ok(BendValue.String(res)) -> value = res
          | _ -> false        
        else
          true
  ]
