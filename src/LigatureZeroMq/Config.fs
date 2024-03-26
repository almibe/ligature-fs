// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.ZMQ.Config

open Ligature.Sqlite.Main
open Argu
open System

type CliArguments =
    | Port of port:string
    | Sqlite of file:string
    | In_Memory

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Port _ -> "specify access port."
            | Sqlite _ -> "use Sqlite3 for storage using the passed file."
            | In_Memory -> "use a temporary in-memeory only store."

type Persistance = | Sqlite of connectionString: LigatureSqliteConfig

type Config =
    { url: string
      persistance: Persistance }

let defaultDBFile () =
    let homeDir = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile)
    let pathCharacter = System.IO.Path.DirectorySeparatorChar.ToString()
    $"{homeDir}{pathCharacter}.ligature{pathCharacter}sqlite.db"

let readConfig () =
    { url = "http://localhost:4200"
      persistance = Sqlite (File (defaultDBFile ())) }
