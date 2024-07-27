// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Test

open Expecto
open Ligature.Main

[<Tests>]
let tests =
    testList
        "Tests"
        [ testCase "Check Triple Equality"
          <| fun _ ->
              Expect.equal
                  ((PatternWord.Word(Word("a"))), (PatternWord.Word(Word("b"))), (Value.Word(Word("c"))))
                  ((PatternWord.Word(Word("a"))), (PatternWord.Word(Word("b"))), (Value.Word(Word("c"))))
                  "" ]

[<EntryPoint>]
let main argv = runTestsInAssemblyWithCLIArgs [] argv
