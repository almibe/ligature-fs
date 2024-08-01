// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Serialization

open Main
open FsToolkit.ErrorHandling
open Wander.Model
open System.Collections
open System.IO
open Wander.Main
open Wander.Model
open Ligature.Wander.Interpreter
//open LigatureStore.InMemoryStore

let readLigature (input: string) : Result<Network, LigatureError> = run (Map.empty) Set.empty input //emptyNetwork

let writeLigature (input: Network) : string =
    let sb = System.Text.StringBuilder()
    sb.Append("{") |> ignore

    Seq.iter
        (fun statement ->
            let s: string = printStatement statement
            sb.Append(s) |> ignore)
        input

    sb.Append("}") |> ignore
    sb.ToString()
