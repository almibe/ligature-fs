// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Infer.Test

open Expecto
open Ligature.Main
open Ligature.InMemoryStore
open TinyDL.NewParser
open TinyDL.Main
open FSharpPlus

let rec allFiles dirs =
    if Seq.isEmpty dirs then
        Seq.empty
    else
        seq {
            yield! dirs |> Seq.collect System.IO.Directory.EnumerateFiles
            yield! dirs |> Seq.collect System.IO.Directory.EnumerateDirectories |> allFiles
        }

[<Tests>]
let scriptTestSuite =
    let ligatureTestSuite =
        System.Environment.GetEnvironmentVariable("LIGATURE_TEST_SUITE")

    if ligatureTestSuite <> null then
        allFiles [ ligatureTestSuite ]
        |> Seq.filter (fun file -> String.endsWith ".tinydl" file)
        |> Seq.map (fun file ->
            let script = System.IO.File.ReadLines file |> String.concat "\n"

            testCase $"Test for {file}"
            <| fun _ ->
                match read script with
                | Ok res ->
                    match interpret res with
                    | Ok _ -> ()
                    | _ -> failwith "TODO"
                | Error(err) -> failwithf "Test failed %A" err)
        |> Seq.toList
        |> testList "tiny-dl tests"
    else
        failwith "Please set LIGATURE_TEST_SUITE environment variable."
