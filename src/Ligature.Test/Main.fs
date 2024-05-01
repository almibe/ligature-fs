// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Test

open Expecto
open Ligature

let ident id =
    match identifier id with
    | Ok id -> id
    | _ -> failwith "Error"

[<Tests>]
let tests =
    testList
        "Tests"
        [ testCase "Create Valid Identifier"
          <| fun _ ->
              match identifier "hello" with
              | Ok id -> Expect.equal (readIdentifier id) "hello" "Identifiers should be equal"
              | Error(_) -> failtest "Error parsing Identifier."
          testCase "Check Statement Equality"
          <| fun _ ->
              Expect.equal
                  (statement (ident "a") (ident "b") (Value.Identifier(ident "c")))
                  (statement (ident "a") (ident "b") (Value.Identifier(ident "c")))
                  "" ]

[<EntryPoint>]
let main argv = runTestsInAssemblyWithCLIArgs [] argv
