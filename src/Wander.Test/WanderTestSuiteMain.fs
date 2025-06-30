// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.TestSuite

open Expecto
open FSharpPlus
open Wander.Main
open Library
open Model
open Ligature.Model
open Ligature.InMemoryStore
open Ligature.Store
open LiteDB

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
    let ligatureTestSuite =
        System.Environment.GetEnvironmentVariable "LIGATURE_TEST_SUITE"

    if ligatureTestSuite <> null then
        allFiles [ ligatureTestSuite ]
        |> Seq.filter (fun file -> String.endsWith ".wander" file)
        |> Seq.map (fun file ->
            let script = System.IO.File.ReadLines file |> String.concat "\n"

            testCase $"Test for {file}"
            <| fun _ ->
                let stores: ILigatureStore list =
                    [ new InMemoryStore(); new LigatureStore(new LiteDatabase ":memory:") ]

                stores
                |> List.iter (fun store ->
                    match run (stdFns store) Map.empty Map.empty script with
                    | Ok(Expression.Assertions result) ->
                        let mutable names: Map<string, string> = Map.empty
                        let mutable comments: Map<string, string> = Map.empty

                        let failures: Set<string> =
                            Set.fold
                                (fun state value ->
                                    match value with
                                    | Assertion.Triple({ value = testId }, Term "name", { value = name }) ->
                                        names <- Map.add testId name names
                                        state
                                    | Assertion.Triple({ value = testId }, Term "state", value) ->
                                        match value with
                                        | { value = "pass" } -> state
                                        | { value = "fail" } -> Set.add testId state
                                        | state -> failwith $"Unexpected state value {state}"
                                    | Assertion.Triple({ value = testId }, Term "comment", { value = comment }) ->
                                        comments <- Map.add testId comment comments
                                        state
                                    | Assertion.Triple(testId, Term "test-group", name) -> state
                                    | x -> failwith $"Unexpected value as test result. {x}")
                                Set.empty
                                result

                        let errorMsg: string =
                            Set.fold
                                (fun state value ->
                                    state + " - " + Map.find value names + " - " + Map.find value comments + "\n")
                                "Failed tests:\n"
                                failures

                        Expect.isEmpty failures errorMsg
                    | Ok x -> failwith $"Unexpected value - {x}"
                    | Error err -> failwithf "Test failed %A" err))
        |> Seq.toList
        |> testList "Wander tests"
    else
        failwith "Please set LIGATURE_TEST_SUITE environment variable."

// [<Tests>]
// let wanderDocsTestSuite =
//     let wanderLibs =
//         System.Environment.GetEnvironmentSlot("WANDER_LIBS")

//     if wanderLibs <> null then
//         let docsFileName = wanderLibs ++ "/" ++ "docs.wander"
//         let script = System.IO.File.ReadLines docsFileName |> String.concat "\n"

//         testCase $"Docs test cases"
//         <| fun _ ->
//             match runWithDefaults script with
//             | Ok (networks, stack) ->
//                 match stack with
//                 | [ Any.Network n ] ->
//                     // search network for test cases -- use Ligature.Core.networkMatch
//                     // run the test case
//                     failwith "TODO"
//                 | _ -> failwith "Error reading docs.wander"
//             | Error(err) -> failwithf "Test failed %A" err
//     else
//         failwith "Please set WANDER_LIBS environment variable."
