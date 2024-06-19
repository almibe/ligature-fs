// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Bindings.Test

open Expecto
open Ligature.Wander.Bindings
open Ligature.Wander.Model

[<Tests>]
let tests =
    testList
        "Binding Tests"
        [ testCase "add a single value and read"
          <| fun _ ->
              let bindings = newBindings ()
              Expect.equal (read "test" bindings) None "Bindings should start empty."
              let bindings = bind "hello" (WanderValue.Int 5) bindings
              Expect.equal (read "hello" bindings) (Some(WanderValue.Int 5)) "Read bindings after adding known value."
              Expect.equal (read "test" bindings) None "Test still should not be bound."
          testCase "test scoping"
          <| fun _ ->
              let bindings = newBindings ()
              Expect.equal (read "test" bindings) None "Bindings should start empty."
              let bindings = bind "hello" (WanderValue.Int 5) bindings
              let bindings = addScope bindings
              Expect.equal (read "hello" bindings) (Some(WanderValue.Int 5)) "Read bindings after adding known value."
              let bindings = bind "hello" (WanderValue.Int 6) bindings

              Expect.equal
                  (read "hello" bindings)
                  (Some(WanderValue.Int 6))
                  "Read bindings after adding known value in new scope."

              let bindings = removeScope bindings
              Expect.equal (read "hello" bindings) (Some(WanderValue.Int 5)) "Read bindings after adding known value." ]
