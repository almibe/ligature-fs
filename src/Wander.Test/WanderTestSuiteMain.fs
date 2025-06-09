// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.TestSuite

open Expecto
open FSharpPlus
open Wander.Main
open Library
open InMemoryStore
open Model
open Ligature.Model

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
        System.Environment.GetEnvironmentVariable("LIGATURE_TEST_SUITE")

    if ligatureTestSuite <> null then
        allFiles [ ligatureTestSuite ]
        |> Seq.filter (fun file -> String.endsWith ".wander" file)
        |> Seq.map (fun file ->
            let script = System.IO.File.ReadLines file |> String.concat "\n"

            testCase $"Test for {file}"
            <| fun _ ->
                match run (stdFns (new InMemoryStore())) Map.empty Map.empty script with
                | Ok(Expression.Tuple results) ->
                    List.iter
                        (fun result ->
                            match result with
                            | Expression.NodeLiteral result ->
                                match result.attributes.TryFind(Term "status") with
                                | Some(Expression.Term(Term "pass")) -> ()
                                | Some(Expression.Term(Term "fail")) -> failwith "TODO"
                                | _ -> failwith "TODO"
                            | x -> printfn $"Unexpected value - {printAny x}")
                        results
                | Ok(Expression.ABox _) -> () //TODO eventually remove
                | Ok x -> printfn $"Unexpected value - {x}"
                | Error err -> failwithf "Test failed %A" err)
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
