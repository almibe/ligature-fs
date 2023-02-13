// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.TestSuite
open Expecto
open Ligature

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

let ds name =
    match dataset name with
    | Ok (ds) -> ds
    | Error (_) -> todo

let ligatureTestSuite (createInstance: Unit -> Ligature) =
    let helloDS = ds "hello"

    testList "Datasets Tests" [
        testCase "start with no Datasets" <| fun _ ->
            let instance = createInstance ()
            let datasets = instance.AllDatasets()
            Expect.equal (datasets) (Ok Array.empty) "Datasets should be empty."
        testCase "add a Dataset" <| fun _ ->
            let instance = createInstance ()
            instance.CreateDataset (helloDS) |> ignore
            let datasets = instance.AllDatasets()
            Expect.equal (datasets) (Ok [|helloDS|]) "Dataset should contain hello Dataset."
        testCase "check if Dataset exists" <| fun _ ->
            let instance = createInstance ()
            Expect.equal (instance.DatasetExists helloDS) (Ok false) "Dataset shouldn't exist before adding."
            instance.CreateDataset (helloDS) |> ignore
            Expect.equal (instance.DatasetExists helloDS) (Ok true) "Dataset should now exist."
        //TODO - add "match datasets prefix exact"
        //TODO - add "match datasets prefix"
        //TODO - add "match datasets range"
        testCase "create and remove Dataset" <| fun _ ->
            let instance = createInstance ()
            todo
    ]
