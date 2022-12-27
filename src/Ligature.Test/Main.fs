module Ligature.Test
open Expecto
open Ligature

[<Tests>]
let tests =
  testList "Identifier Tests" [
    testCase "Create Valid Identifier" <| fun _ ->
      let id = identifier("hello")
      Expect.equal (readIdentifier id) "hello" "Identifiers should be equal"
  ]

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly defaultConfig argv
