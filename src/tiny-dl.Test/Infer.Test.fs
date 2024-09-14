// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Infer.Test

open Expecto
open TinyDL.Main

[<Tests>]
let tests =
    testList
        "Check Tests"
        [ testCase "Call check on empty arguments"
          <| fun _ -> Expect.isTrue (check Set.empty Set.empty) ""
          testCase "Call check with empty TBox"
          <| fun _ ->
              Expect.isTrue
                  (check
                      Set.empty
                      (Set.ofList
                          [ UnaryPredicate
                                { symbol = "betty"
                                  concept = AtomicConcept "Cat" } ]))
                  "" ]
// "Infer Tests"
// [ testCase "Check Infer on empty arguments"
//   <| fun _ ->
//       Expect.equal
//           ()
//           ()
//           "" ]