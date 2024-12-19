// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

open Ligature.Model
open Fable.Core.JsInterop
open Wander.Main
open Wander.Commands
open Wander.Model
open Wander.Lib
open System.Collections.Generic

let runScript (script: string) =
    match run stdCommands (emptyVariables ()) script with
    | Ok(Some res) -> prettyPrint res
    | Ok _ -> "{}"
    | _ -> failwith "TODO"

let runScriptResult (script: string) =
    match run stdCommands (emptyVariables ()) script with
    | Ok(Some(Any.ResultSet resultSet)) ->
        let mutable result = [||]
        Set.iter (fun res ->
            let dict = new Dictionary<string, string>()
            Map.iter (fun k v ->
                match k,v with
                | Variable v, Value.Element (Element e) -> dict.Add(v, e)
                | Variable v, Value.Literal l -> dict.Add(v, l)
                | Variable v, Value.Variable (Variable variable) -> dict.Add(v, variable)) res
            result <- Array.append result [|dict|]) resultSet
        result
    | _ -> failwith "Script must return a ResultSet when you call runScriptResult."
