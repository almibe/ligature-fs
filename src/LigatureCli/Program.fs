// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Cli

open Wander.Main
open Wander.Library
open Wander.Model
open Ligature.InMemoryStore
open System
open Wander.Parser
open Wander.Tokenizer
open Store

[<EntryPoint>]
let main (args: string[]) =
    let script =
        if args[0] = "-i" then
            printfn "Input: "
            Console.ReadLine()
        else
            let dir = IO.Directory.GetCurrentDirectory()
            let file = $"{dir}/{args[0]}"
            System.IO.File.ReadAllText file

    match tokenize script with
    | Ok script ->
        printfn "Tokens: %A\n\n" script

        match parse script with
        | Ok res -> printfn $"AST:\n{res}\n\n"

    let store = inMemoryStore () //new InMemoryStore()

    match run (stdFns store) Map.empty Map.empty script with
    | Ok res -> printfn $"{printExpression res}"
    | Error err -> printfn $"{err.UserMessage}"

    0
