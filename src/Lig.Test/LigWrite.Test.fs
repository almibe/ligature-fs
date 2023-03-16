// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module LigTestWrite

open Expecto
open Ligature
open Ligature.Lig.Write

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

let unwrap result =
    Result.defaultWith (fun _ -> todo) result

[<Tests>]
let tests =
    testList
        "Lig Write Suite"
        [ testCase "Write Identifier"
          <| fun _ -> Expect.equal (writeIdentifier (unwrap (identifier ("hello")))) "<hello>" ""
          testCase "Write Value"
          <| fun _ -> Expect.equal (writeValue (Identifier(unwrap (identifier ("hello"))))) "<hello>" "" // identifier
          //assertEq(writeValue(String("hello")), "\"hello\"") // string
          //Expect.equal (writeValue(Integer(4567L)), "4567") // integer
          //assertEq(writeValue(unwrap(identifier("hello"))), "<hello>") // bytes
          ]
