// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Test.Instance

open Expecto
open Ligature
open Ligature.Wander.Model
open Ligature.Wander.Main

let ident id =
    WanderValue.Identifier(
        match identifier id with
        | Ok v -> v
        | Error _ -> todo
    )

let mutable backend: ILigature = InMemory.LigatureInMemory ()
let bindings () = Wander.Preludes.instancePrelude backend

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
              let result = run script (bindings())
              Expect.equal result (Ok(WanderValue.Tuple[])) ""
          testCase "Writing Statements to a new Dataset"
          <| fun _ ->
              let script = """
              createDataset("hello")
              write("hello" ((<a> <b> <c>)))
              allStatements("hello")
              """
              let result = run script (bindings())
              Expect.equal result (Ok(WanderValue.Tuple[WanderValue.Tuple[(ident "a"); (ident "b"); (ident "c")]])) ""
          testCase "Calling createDataset shouldn't affect Datasets that already exist"
          <| fun _ ->
              let script = """
              createDataset("hello")
              write("hello" ((<a> <b> <c>)))
              createDataset("hello")
              allStatements("hello")
              """
              let result = run script (bindings())
              Expect.equal result (Ok(WanderValue.Tuple[WanderValue.Tuple[(ident "a"); (ident "b"); (ident "c")]])) ""
          testCase "Writing Statements to a new Dataset and calling match"
          <| fun _ ->
              let script = """
              createDataset("hello")
              write("hello" ((<a> <b> <c>) (<d> <e> <f>)))
              match("hello" <a> <b> <c>)
              """
              let result = run script (bindings())
              Expect.equal result (Ok(WanderValue.Tuple[WanderValue.Tuple[(ident "a"); (ident "b"); (ident "c")]])) ""
        ]
