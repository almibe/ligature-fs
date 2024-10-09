// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module TinyDL.Main

open Model
open Tokenizer
open NewParser
open Ligature.Main

let interpret ((description, network, checks): Script) : Result<Network, TinyDLError> =
    match infer description network with
    | Ok res -> Ok res
    | _ -> failwith "TODO"

let read (script: string) : Result<Script, TinyDLError> =
    match tokenize script with
    | Ok res ->
        match parse res with
        | Ok res -> express res
        | Error errorValue -> Error errorValue
    | Error errorValue -> Error errorValue
