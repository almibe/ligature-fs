// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

open Ligature.Model
open Fable.Core.JsInterop
open Wander.Main
open Wander.Commands
open Wander.Model
open Wander.Lib

let runScript (script: string) =
    match run stdCommands (emptyVariables ()) script with
    | Ok(Some res) -> prettyPrint res
    | Ok _ -> "{}"
    | _ -> failwith "TODO"

let readValue (input: string) =
    match read input with
    | Ok result ->
        match result with
        | Any.Element(Element e) -> e
        | Any.Quote _ -> failwith "TODO"
        | Any.Network network -> failwith "TODO" //networkToJs network
        | Any.Literal literal -> encodeString literal
        | Any.Variable(Variable variable) -> variable
        | Any.Pipe -> failwith "TODO"
        | Any.ResultSet rs -> failwith "TODO"
    | _ -> failwith "Error reading value."
