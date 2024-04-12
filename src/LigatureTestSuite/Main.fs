// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.TestSuite

open Expecto
open Ligature
open FSharpPlus
open Ligature.Wander.Main
open Ligature.Wander.Lib.Preludes

/// Unsafe helper function for creating Identifiers.
let id ident =
    match identifier ident with
    | Ok(i) -> i
    | Error(_) -> failwith "error making Identifier"

/// Unsafe helper function for creating statements for testing.
let statement (entity: string) (attribute: string) (value: Value) =
    let e = id entity
    let a = id attribute

    { Entity = e
      Attribute = a
      Value = value }

let rec allFiles dirs =
    if Seq.isEmpty dirs then
        Seq.empty
    else
        seq {
            yield! dirs |> Seq.collect System.IO.Directory.EnumerateFiles
            yield! dirs |> Seq.collect System.IO.Directory.EnumerateDirectories |> allFiles
        }

let wanderTestSuite (createInstance: Unit -> ILigature) =
    let ligatureTestSuite =
        System.Environment.GetEnvironmentVariable("LIGATURE_TEST_SUITE")

    if ligatureTestSuite <> null then
        allFiles [ ligatureTestSuite ]
        |> Seq.filter (fun file -> String.endsWith ".wander" file)
        |> Seq.map (fun file ->
            let script = System.IO.File.ReadLines file |> String.concat "\n"

            testCase $"Test for {file}"
            <| fun _ ->
                match run script (instancePrelude (createInstance ())) with
                | Ok(_) -> ()
                | Error(err) -> failwithf "Test failed %A" err)
        |> Seq.toList
        |> testList "Wander tests"
    else
        failwith "Please set LIGATURE_TEST_SUITE environment variable."

let ligTestSuite (createInstance: Unit -> ILigature) =
    let ligatureTestSuite =
        System.Environment.GetEnvironmentVariable("LIGATURE_TEST_SUITE")

    if ligatureTestSuite <> null then
        allFiles [ ligatureTestSuite ]
        |> Seq.filter (fun file -> String.endsWith ".lig" file)
        |> Seq.map (fun file ->
            let script = System.IO.File.ReadAllLines file

            testCase $"Test for {file}"
            <| fun _ ->
                let instance = new Ligature.InMemory.LigatureInMemory()
                instance.LoadFromString(script)
                let datasets = (instance :> ILigature).AllDatasets()
                Expect.equal (Ok [ DatasetName "hello" ]) datasets "")
        |> Seq.toList
        |> testList "Lig tests"
    else
        failwith "Please set LIGATURE_TEST_SUITE environment variable."

let ligatureTestSuite (createInstance: Unit -> ILigature) =
    let helloDS = DatasetName "hello"
    let hello2DS = DatasetName "hello2"
    let hello3DS = DatasetName "hello3"

    let jvName = statement "character:1" "name" (Value.String "Jean Valjean")
    let jvNumber = statement "character:1" "prisonerNumber" (Value.Integer 24601)
    let javertName = statement "character:2" "name" (Value.String "Inspector Javert")

    let nemesis =
        statement "character:2" "hasNemesis" (id "character:1" |> Value.Identifier)

    let statements = List.sort [ jvName; jvNumber; javertName; nemesis ]

    testList
        "Ligature Test Suite"
        [ testCase "start with no Datasets"
          <| fun _ ->
              let instance = createInstance ()
              let datasets = instance.AllDatasets()
              Expect.equal datasets (Ok []) "Datasets should be empty."
          testCase "add a Dataset"
          <| fun _ ->
              let instance = createInstance ()
              instance.CreateDataset(helloDS) |> ignore
              let datasets = instance.AllDatasets()
              Expect.equal (datasets) (Ok [ helloDS ]) "Dataset should contain hello Dataset."
          testCase "check if Dataset exists"
          <| fun _ ->
              let instance = createInstance ()
              Expect.equal (instance.DatasetExists helloDS) (Ok false) "Dataset shouldn't exist before adding."
              instance.CreateDataset(helloDS) |> ignore
              Expect.equal (instance.DatasetExists helloDS) (Ok true) "Dataset should now exist."
          //TODO - add "match datasets prefix exact"
          //TODO - add "match datasets prefix"
          //TODO - add "match datasets range"
          testCase "create and remove new Dataset"
          <| fun _ ->
              let instance = createInstance ()
              instance.CreateDataset helloDS |> ignore
              instance.CreateDataset hello2DS |> ignore
              instance.RemoveDataset helloDS |> ignore
              instance.RemoveDataset hello3DS |> ignore
              let datasets = instance.AllDatasets()
              Expect.equal datasets (Ok [ hello2DS ]) "Dataset should only contain hello2 Dataset."

          testCase "new Dataset should be empty"
          <| fun _ ->
              let instance = createInstance ()
              instance.CreateDataset(helloDS) |> ignore
              let result = instance.AllStatements helloDS
              Expect.equal result (Ok []) "Newly created Dataset should be empty."
          testCase "add Statement with Identifier Value to instance"
          <| fun _ ->
              let instance = createInstance ()
              Expect.isOk (instance.CreateDataset(helloDS)) ""
              let statement = statement "a" "b" (Value.Identifier(id "a"))
              let writeRes = instance.AddStatements helloDS [ statement ]
              Expect.isOk writeRes "Could not write statements."
              let result = instance.AllStatements helloDS
              Expect.equal result (Ok [ statement ]) "Dataset should contain new Statements."
          testCase "add Statement with Integer Value to instance"
          <| fun _ ->
              let instance = createInstance ()
              Expect.isOk (instance.CreateDataset(helloDS)) ""
              let statement = statement "a" "b" (Value.Integer 12345)
              let writeRes = instance.AddStatements helloDS [ statement ]
              Expect.isOk writeRes "Could not write statements."
              let result = instance.AllStatements helloDS
              Expect.equal result (Ok [ statement ]) "Dataset should contain new Statements."
          testCase "add Statement with String Value to instance"
          <| fun _ ->
              let instance = createInstance ()
              Expect.isOk (instance.CreateDataset(helloDS)) ""
              let statement = statement "a" "b" (Value.String "hello, world")
              let writeRes = instance.AddStatements helloDS [ statement ]
              Expect.isOk writeRes "Could not write statements."
              let result = instance.AllStatements helloDS
              Expect.equal result (Ok [ statement ]) "Dataset should contain new Statements."
          testCase "add multiple Statements to Dataset"
          <| fun _ ->
              let instance = createInstance ()
              Expect.isOk (instance.CreateDataset(helloDS)) ""
              let writeRes = instance.AddStatements helloDS statements
              Expect.isOk writeRes "Could not write statements."
              let result = instance.AllStatements helloDS
              let result = Result.map (fun statements -> List.sort statements) result
              Expect.equal result (Ok statements) "Dataset should contain new Statements."
          testCase "add new Statements to Dataset with dupes"
          <| fun _ ->
              let instance = createInstance ()
              instance.CreateDataset(helloDS) |> ignore

              ignore <| instance.AddStatements helloDS statements
              ignore <| instance.AddStatements helloDS statements
              ignore <| instance.AddStatements helloDS statements

              let result = instance.AllStatements helloDS
              let result = Result.map (fun statements -> List.sort statements) result
              Expect.equal result (Ok statements) "Dataset should contain new Statements."
          //TODO - add new Identifier test
          testCase "removing Statements from Dataset"
          <| fun _ ->
              let instance = createInstance ()
              instance.CreateDataset(helloDS) |> ignore

              ignore <| instance.AddStatements helloDS statements
              ignore <| instance.RemoveStatements helloDS [ nemesis ]

              let result = instance.AllStatements helloDS
              let result = Result.map (fun statements -> List.sort statements) result

              Expect.equal
                  result
                  (Ok(List.filter (fun statement -> statement = nemesis |> not) statements))
                  "Dataset should contain all but removed Statements."
          testCase "matching Statements in Dataset"
          <| fun _ ->
              let instance = createInstance ()
              instance.CreateDataset(helloDS) |> ignore

              ignore <| instance.AddStatements helloDS statements

              let results = instance.Query helloDS (fun tx -> tx.MatchStatements None None None)
              let results = Result.map (fun statements -> List.sort statements) results
              Expect.equal results (Ok statements) ""
          //TODO add more query cases
          ]
