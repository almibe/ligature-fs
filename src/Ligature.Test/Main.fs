module Ligature.Test

open Expecto
open Ligature

[<Tests>]
let tests =
    testList
        "Identifier Tests"
        [ testCase "Create Valid Identifier"
          <| fun _ ->
              match identifier "hello" with
              | Ok id -> Expect.equal (readIdentifier id) "hello" "Identifiers should be equal"
              | Error(_) -> failtest "Error parsing Identifier." ]

[<EntryPoint>]
let main argv =
    runTestsInAssemblyWithCLIArgs [] argv
