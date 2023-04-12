// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Instance.Test

open Expecto
open Ligature
open Ligature.Wander.Model
open Ligature.Wander.Main

let ident id =
    Identifier(
        match identifier id with
        | Ok(v) -> v
        | Error(_) -> todo
    )

//TODO move this to test setup
let instance = InMemory.LigatureInMemory ()
let bindings = Wander.Preludes.instancePrelude instance

[<Tests>]
let tests =
    testList
        "Instace Tests"
        [ testCase "Calling allStatements on a new Dataset"
          <| fun _ ->
              let script = """
              createDataset("hello")
              allStatements("hello")
              """
              let result = run script bindings
              Expect.equal result (Ok(Tuple[])) ""
          testCase "Writing Statements to a new Dataset"
          <| fun _ ->
              let script = """
              createDataset("hello")
              write("hello" ((<a> <b> <c>)))
              allStatements("hello")
              """
              let result = run script bindings
              Expect.equal result (Ok(Tuple[Tuple[(ident "a"); (ident "b"); (ident "c")]])) ""
        ]
