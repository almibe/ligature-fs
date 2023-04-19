// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Test.Main

open Expecto
open Ligature
open Argu
open Ligature.Sqlite.Main

type CliArguments =
    | InMemory
    | SqliteInMemory

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | InMemory -> "run tests using in-memeory backend (default)."
            | SqliteInMemory -> "run tests using Sqlite3 in-memory backend."

[<EntryPoint>]
let main argv =
    let reader = EnvironmentVariableConfigurationReader() :> IConfigurationReader
    let parser =  ArgumentParser.Create<CliArguments>()
    let results = parser.Parse(argv, configurationReader=reader).GetAllResults()
    match results with
    | [] | [InMemory] -> Instance.backend <- (fun () -> InMemory.LigatureInMemory () )
    | [SqliteInMemory] -> Instance.backend <- (fun () -> ligatureSqlite LigatureSqliteConfig.InMemory )
    | _ -> failwith $"Unexpected configuration. {results}"    
    runTestsInAssembly defaultConfig [||]//argv
