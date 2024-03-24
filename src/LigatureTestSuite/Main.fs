// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.TestSuite

open Expecto
open Ligature
open FSharpPlus
open Ligature.Bend.Main
open Ligature.Bend.Lib.Preludes

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

/// Unsafe helper function for creating Identifiers.
let id ident =
    match identifier ident with
    | Ok(i) -> i
    | Error(_) -> todo

/// Unsafe helper function for creating statements for testing.
let statement (entity: string) (attribute: string) (value: Value) =
    let e = id entity
    let a = id attribute

    { Entity = e
      Attribute = a
      Value = value }

let rec allFiles dirs =
    if Seq.isEmpty dirs then Seq.empty else
        seq { yield! dirs |> Seq.collect System.IO.Directory.EnumerateFiles
              yield! dirs |> Seq.collect System.IO.Directory.EnumerateDirectories |> allFiles }

let bendTestSuite (createInstance: Unit -> ILigature) =
    let ligatureTestSuite = System.Environment.GetEnvironmentVariable("LIGATURE_TEST_SUITE")
    // printf "%A" ligatureTestSuite
    // printf "%A" (allFiles [ligatureTestSuite] |> Seq.filter (fun file -> printf "%s" file; String.endsWith ".bend" file ))
    if ligatureTestSuite <> null then
        allFiles [ligatureTestSuite]
        |> Seq.filter (fun file -> String.endsWith ".bend" file)
        |> Seq.map (fun file ->
            let script = System.IO.File.ReadLines file |> String.concat "\n"
            testCase $"Test for {file}"
            <| fun _ ->
                match run script (standardPrelude ()) with
                | Ok(_) -> ()
                | Error(err) -> failwith "Test failed")
        |> Seq.toList
        |> testList "Bend tests"
    else
        failwith "Please set LIGATURE_TEST_SUITE environment variable."

let ligatureTestSuite (createInstance: Unit -> ILigature) =
    let helloDS = Dataset "hello"
    let hello2DS = Dataset "hello2"
    let hello3DS = Dataset "hello3"

    let jvName = statement "character:1" "name" (String "Jean Valjean")
    let jvNumber = statement "character:1" "prisonerNumber" (Integer 24601)
    let javertName = statement "character:2" "name" (String "Inspector Javert")
    let nemesis = statement "character:2" "hasNemesis" (id "character:1" |> Identifier)
    let statements = List.sort [ jvName; jvNumber; javertName; nemesis ]

    testList
        "Ligature Test Suite"
        [ testCase "start with no Datasets"
          <| fun _ ->
              let instance = createInstance ()
              let datasets = instance.AllDatasets()
              Expect.equal (datasets) (Ok List.empty) "Datasets should be empty."
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
              let result = instance.Query helloDS (fun tx -> tx.AllStatements())
              Expect.equal result (Ok []) "Newly created Dataset should be empty."
          testCase "add Statement with Identifier Value to instance"
          <| fun _ ->
              let instance = createInstance ()
              Expect.isOk (instance.CreateDataset(helloDS)) ""
              let statement = statement "a" "b" (Identifier(id "a"))
              let writeRes = instance.Write helloDS (fun tx -> tx.AddStatement statement)
              Expect.isOk writeRes "Could not write statements."
              let result = instance.Query helloDS (fun tx -> tx.AllStatements())
              Expect.equal result (Ok [ statement ]) "Dataset should contain new Statements."
          testCase "add Statement with Integer Value to instance"
          <| fun _ ->
              let instance = createInstance ()
              Expect.isOk (instance.CreateDataset(helloDS)) ""
              let statement = statement "a" "b" (Integer 12345)
              let writeRes = instance.Write helloDS (fun tx -> tx.AddStatement statement)
              Expect.isOk writeRes "Could not write statements."
              let result = instance.Query helloDS (fun tx -> tx.AllStatements())
              Expect.equal result (Ok [ statement ]) "Dataset should contain new Statements."
          testCase "add Statement with String Value to instance"
          <| fun _ ->
              let instance = createInstance ()
              Expect.isOk (instance.CreateDataset(helloDS)) ""
              let statement = statement "a" "b" (String "hello, world")
              let writeRes = instance.Write helloDS (fun tx -> tx.AddStatement statement)
              Expect.isOk writeRes "Could not write statements."
              let result = instance.Query helloDS (fun tx -> tx.AllStatements())
              Expect.equal result (Ok [ statement ]) "Dataset should contain new Statements."
          testCase "add multiple Statements to Dataset"
          <| fun _ ->
              let instance = createInstance ()
              Expect.isOk (instance.CreateDataset(helloDS)) ""

              let writeRes =
                  instance.Write helloDS (fun tx ->
                      List.iter (fun statement -> tx.AddStatement statement |> ignore) statements
                      Ok())

              Expect.isOk writeRes "Could not write statements."
              let result = instance.Query helloDS (fun tx -> tx.AllStatements())
              let result = Result.map (fun statements -> List.sort statements) result
              Expect.equal result (Ok statements) "Dataset should contain new Statements."
          testCase "add new Statements to Dataset with dupes"
          <| fun _ ->
              let instance = createInstance ()
              instance.CreateDataset(helloDS) |> ignore

              instance.Write helloDS (fun tx ->
                  List.iter (fun statement -> tx.AddStatement statement |> ignore) statements
                  List.iter (fun statement -> tx.AddStatement statement |> ignore) statements
                  List.iter (fun statement -> tx.AddStatement statement |> ignore) statements
                  Ok())
              |> ignore

              let result = instance.Query helloDS (fun tx -> tx.AllStatements())
              let result = Result.map (fun statements -> List.sort statements) result
              Expect.equal result (Ok statements) "Dataset should contain new Statements."
          //TODO - add new Identifier test
          testCase "removing Statements from Dataset"
          <| fun _ ->
              let instance = createInstance ()
              instance.CreateDataset(helloDS) |> ignore

              instance.Write helloDS (fun tx ->
                  List.iter (fun statement -> tx.AddStatement statement |> ignore) statements
                  tx.RemoveStatement nemesis |> ignore
                  Ok())
              |> ignore

              let result = instance.Query helloDS (fun tx -> tx.AllStatements())
              let result = Result.map (fun statements -> List.sort statements) result

              Expect.equal
                  result
                  (Ok(List.filter (fun statement -> statement = nemesis |> not) statements))
                  "Dataset should contain all but removed Statements."
          testCase "matching Statements in Dataset"
          <| fun _ ->
              let instance = createInstance ()
              instance.CreateDataset(helloDS) |> ignore

              instance.Write helloDS (fun tx ->
                  List.iter (fun statement -> tx.AddStatement statement |> ignore) statements
                  Ok())
              |> ignore

              let results = instance.Query helloDS (fun tx -> tx.MatchStatements None None None)
              let results = Result.map (fun statements -> List.sort statements) results
              Expect.equal results (Ok statements) ""
          //TODO add more query cases
          ]
