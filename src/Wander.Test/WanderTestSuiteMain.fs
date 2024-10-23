// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.TestSuite

open Expecto
open FSharpPlus
open Wander.Main
open Lib
open Ligature.InMemoryStore

let rec allFiles dirs =
    if Seq.isEmpty dirs then
        Seq.empty
    else
        seq {
            yield! dirs |> Seq.collect System.IO.Directory.EnumerateFiles
            yield! dirs |> Seq.collect System.IO.Directory.EnumerateDirectories |> allFiles
        }

[<Tests>]
let wanderTestSuite =
    //    let createBindings () = coreBindings //(Ligature.LigatureStore.InMemoryStore.empty ())

    let ligatureTestSuite =
        System.Environment.GetEnvironmentVariable("LIGATURE_TEST_SUITE")

    if ligatureTestSuite <> null then
        allFiles [ ligatureTestSuite ]
        |> Seq.filter (fun file -> String.endsWith ".wander" file)
        |> Seq.map (fun file ->
            let script = System.IO.File.ReadLines file |> String.concat "\n"

            testCase $"Test for {file}"
            <| fun _ ->
                match run stdCommands (emptyInMemoryStore ()) script with
                | Ok _ -> ()
                | Error(err) -> failwithf "Test failed %A" err)
        |> Seq.toList
        |> testList "Wander tests"
    else
        failwith "Please set LIGATURE_TEST_SUITE environment variable."
