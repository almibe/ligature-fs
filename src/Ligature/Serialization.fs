// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Serialization

open Main
open InMemoryNetwork
open Wander.Lexer
open FsToolkit.ErrorHandling
open Wander.Model
open System.Collections
open System.IO
open Wander.Main
open Wander.Bindings

let readLigature (input: string): Result<INetwork, LigatureError> =
    match run input (newBindings ()) with
    | Ok(WanderValue.Pattern(res)) -> 
        match res.ToNetwork with
        | Some res -> Ok(res)
        | _ -> failwith "TODO"
    | _ -> failwith "Error"

let writeLigature (input: INetwork): string =
    let sb = new System.Text.StringBuilder()
    sb.Append("{")
    Seq.iter (fun statement ->
        let s: string = printStatement statement
        sb.Append(s) |> ignore) (input.all())
    sb.Append("}")
    sb.ToString()
