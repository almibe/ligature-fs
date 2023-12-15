// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.TestSuite

open Expecto
open Ligature

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

/// Unsafe helper function for creating Identifiers.
let id ident =
    match label ident with
    | Ok(i) -> i
    | Error(_) -> todo

/// Unsafe helper function for creating statements for testing.
let statement (entity: string) (attribute: string) (value: Value) =
    let e = id entity
    let a = id attribute

    { Source = e
      Label = a
      Target = value }

let ligatureTestSuite (createInstance: Unit -> ILigature) =
    let helloDS = Graph "hello"
    let hello2DS = Graph "hello2"
    let hello3DS = Graph "hello3"

    let jvName = statement "character:1" "name" (String "Jean Valjean")
    let jvNumber = statement "character:1" "prisonerNumber" (Integer 24601)
    let javertName = statement "character:2" "name" (String "Inspector Javert")
    let nemesis = statement "character:2" "hasNemesis" (id "character:1" |> Label)
    let statements = List.sort [ jvName; jvNumber; javertName; nemesis ]

    testList
        "Ligature Test Suite"
        [ testCase "start with no Datasets"
          <| fun _ ->
              let instance = createInstance ()
              let datasets = instance.AllGraphs()
              Expect.equal (datasets) (Ok List.empty) "Datasets should be empty."
          testCase "add a Dataset"
          <| fun _ ->
              let instance = createInstance ()
              instance.CreateGraph(helloDS) |> ignore
              let datasets = instance.AllGraphs()
              Expect.equal (datasets) (Ok [ helloDS ]) "Dataset should contain hello Dataset."
          testCase "check if Dataset exists"
          <| fun _ ->
              let instance = createInstance ()
              Expect.equal (instance.GraphExists helloDS) (Ok false) "Dataset shouldn't exist before adding."
              instance.CreateGraph(helloDS) |> ignore
              Expect.equal (instance.GraphExists helloDS) (Ok true) "Dataset should now exist."
          //TODO - add "match datasets prefix exact"
          //TODO - add "match datasets prefix"
          //TODO - add "match datasets range"
          testCase "create and remove new Dataset"
          <| fun _ ->
              let instance = createInstance ()
              instance.CreateGraph helloDS |> ignore
              instance.CreateGraph hello2DS |> ignore
              instance.RemoveGraph helloDS |> ignore
              instance.RemoveGraph hello3DS |> ignore
              let datasets = instance.AllGraphs()
              Expect.equal datasets (Ok [ hello2DS ]) "Dataset should only contain hello2 Dataset."

          testCase "new Dataset should be empty"
          <| fun _ ->
              let instance = createInstance ()
              instance.CreateGraph(helloDS) |> ignore
              let result = instance.Query helloDS (fun tx -> tx.AllEdges())
              Expect.equal result (Ok []) "Newly created Dataset should be empty."
          testCase "add Statement with Identifier Value to instance"
          <| fun _ ->
              let instance = createInstance ()
              Expect.isOk (instance.CreateGraph(helloDS)) ""
              let statement = statement "a" "b" (Label(id "a"))
              let writeRes = instance.Write helloDS (fun tx -> tx.AddEdge statement)
              Expect.isOk writeRes "Could not write statements."
              let result = instance.Query helloDS (fun tx -> tx.AllEdges())
              Expect.equal result (Ok [ statement ]) "Dataset should contain new Statements."
          testCase "add Statement with Integer Value to instance"
          <| fun _ ->
              let instance = createInstance ()
              Expect.isOk (instance.CreateGraph(helloDS)) ""
              let statement = statement "a" "b" (Integer 12345)
              let writeRes = instance.Write helloDS (fun tx -> tx.AddEdge statement)
              Expect.isOk writeRes "Could not write statements."
              let result = instance.Query helloDS (fun tx -> tx.AllEdges())
              Expect.equal result (Ok [ statement ]) "Dataset should contain new Statements."
          testCase "add Statement with String Value to instance"
          <| fun _ ->
              let instance = createInstance ()
              Expect.isOk (instance.CreateGraph(helloDS)) ""
              let statement = statement "a" "b" (String "hello, world")
              let writeRes = instance.Write helloDS (fun tx -> tx.AddEdge statement)
              Expect.isOk writeRes "Could not write statements."
              let result = instance.Query helloDS (fun tx -> tx.AllEdges())
              Expect.equal result (Ok [ statement ]) "Dataset should contain new Statements."
          testCase "add multiple Statements to Dataset"
          <| fun _ ->
              let instance = createInstance ()
              Expect.isOk (instance.CreateGraph(helloDS)) ""

              let writeRes =
                  instance.Write helloDS (fun tx ->
                      List.iter (fun statement -> tx.AddEdge statement |> ignore) statements
                      Ok())

              Expect.isOk writeRes "Could not write statements."
              let result = instance.Query helloDS (fun tx -> tx.AllEdges())
              let result = Result.map (fun statements -> List.sort statements) result
              Expect.equal result (Ok statements) "Dataset should contain new Statements."
          testCase "add new Statements to Dataset with dupes"
          <| fun _ ->
              let instance = createInstance ()
              instance.CreateGraph(helloDS) |> ignore

              instance.Write helloDS (fun tx ->
                  List.iter (fun statement -> tx.AddEdge statement |> ignore) statements
                  List.iter (fun statement -> tx.AddEdge statement |> ignore) statements
                  List.iter (fun statement -> tx.AddEdge statement |> ignore) statements
                  Ok())
              |> ignore

              let result = instance.Query helloDS (fun tx -> tx.AllEdges())
              let result = Result.map (fun statements -> List.sort statements) result
              Expect.equal result (Ok statements) "Dataset should contain new Statements."
          //TODO - add new Identifier test
          testCase "removing Statements from Dataset"
          <| fun _ ->
              let instance = createInstance ()
              instance.CreateGraph(helloDS) |> ignore

              instance.Write helloDS (fun tx ->
                  List.iter (fun statement -> tx.AddEdge statement |> ignore) statements
                  tx.RemoveEdge nemesis |> ignore
                  Ok())
              |> ignore

              let result = instance.Query helloDS (fun tx -> tx.AllEdges())
              let result = Result.map (fun statements -> List.sort statements) result

              Expect.equal
                  result
                  (Ok(List.filter (fun statement -> statement = nemesis |> not) statements))
                  "Dataset should contain all but removed Statements."
          testCase "matching Statements in Dataset"
          <| fun _ ->
              let instance = createInstance ()
              instance.CreateGraph(helloDS) |> ignore

              instance.Write helloDS (fun tx ->
                  List.iter (fun statement -> tx.AddEdge statement |> ignore) statements
                  Ok())
              |> ignore

              let results = instance.Query helloDS (fun tx -> tx.MatchEdges None None None)
              let results = Result.map (fun statements -> List.sort statements) results
              Expect.equal results (Ok statements) ""
          //TODO add more query cases
          ]
