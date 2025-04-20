// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.PrettyPrinter.Test

open Expecto
open Ligature.Model
open Wander.Model

[<Tests>]
let tests =
    testList
        "Pretty Printer Tests"
        [ testCase "Print Text"
          <| fun _ -> Expect.equal (format (PrintToken.Text "Text")) "Text" ""
          testCase "Print Break" <| fun _ -> Expect.equal (format PrintToken.Break) "" ""
          testCase "Print empty Group"
          <| fun _ -> Expect.equal (format (PrintToken.Group [])) "" ""
          testCase "Print small group"
          <| fun _ -> Expect.equal (format (PrintToken.Group [ PrintToken.Text "a"; PrintToken.Text "b" ])) "a b" "" ]
