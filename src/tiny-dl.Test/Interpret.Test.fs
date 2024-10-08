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

let unsafeRead (input: string) =
    match read input with
    | Ok(nodes) ->
        match express nodes with
        | Ok(description, network) ->
            match infer description network with
            | Ok res -> res
            | Error err -> failwith err
        | Error err -> failwith err
    | Error err -> failwith err

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

            testCase $"Test for {file}" <| fun _ -> failwith "TODO")
        // match run script with
        // | Ok _ -> ()
        // | Error(err) -> failwithf "Test failed %A" err)
        |> Seq.toList
        |> testList "Wander tests"
    else
        failwith "Please set LIGATURE_TEST_SUITE environment variable."
