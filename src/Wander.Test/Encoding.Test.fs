// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Test.Encoding

open Expecto
open Ligature.Wander.Main
open Ligature.Wander.Lib.Preludes
open Ligature.Wander.Model

let config =
    { FsCheckConfig.defaultConfig with
        maxTest = 10000 }

[<Tests>]
let properties =
    testList
        "Encoding and Decoding Tests"
        [ 
        //   testProperty "Encode and decode Int"
        //   <| fun (value: bigint) ->
        //       match run (prettyPrint (WanderValue.Int(value))) (standardPrelude ()) with
        //       | Ok(WanderValue.Int(res)) -> value = res
        //       | _ -> false
          testProperty "Encode and decode String"
          <| fun (value: string) ->
              if value <> null then
                  match run (prettyPrint (WanderValue.String(value))) (standardPrelude ()) with
                  | Ok(WanderValue.String(res)) -> value = res
                  | _ -> false
              else
                  true ]
