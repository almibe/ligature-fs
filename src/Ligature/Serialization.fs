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

let readLigature (input: string) : Result<Network, LigatureError> =
    match run input Map.empty List.empty with
    | Ok([ WanderValue.Network(res) ]) -> Ok(res)
    | _ -> failwith "Error"

let writeLigature (input: Network) : string =
    let sb = System.Text.StringBuilder()
    sb.Append("{") |> ignore

    Seq.iter
        (fun triple ->
            let s: string = printTriple triple
            sb.Append(s) |> ignore)
        (input.Write())

    sb.Append("}") |> ignore
    sb.ToString()
