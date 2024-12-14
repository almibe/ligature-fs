// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

open Ligature.Main
open Fable.Core.JsInterop
open Wander.Main
open Wander.Commands
open Wander.Model
open Wander.Lib

let networkToJs (network: Network) =
    let res = createEmpty
    let mutable entries = Set.empty

    Set.iter
        (fun (entry: Entry) ->
            match entry with
            | Entry.Extends { element = Element element
                              concept = Element concept } ->
                let elementJs = createEmpty
                elementJs?first <- element
                elementJs?second <- ":"
                elementJs?third <- concept
                entries <- Set.add elementJs entries
            | Entry.NotExtends { element = Element element
                                 concept = Element concept } ->
                let elementJs = createEmpty
                elementJs?first <- element
                elementJs?second <- "¬:"
                elementJs?third <- concept
                entries <- Set.add elementJs entries
            | Entry.Attribute { element = Element element
                                attribute = Element attribute
                                value = value } ->
                let elementJs = createEmpty
                elementJs?first <- element
                elementJs?second <- attribute

                elementJs?third <-
                    match value with
                    | Value.Element(Element element) -> element
                    | Value.Literal literal -> encodeString literal

                entries <- Set.add elementJs entries)
        network

    res?entries <- Set.toArray entries
    res

let runScript (script: string) =
    match run stdCommands (emptyVariables()) script with
    | Ok (Some res) -> prettyPrint res
    | Ok _ -> "{}"
    | _ -> failwith "TODO"

let readValue (input: string) =
    match read input with
    | Ok result ->
        match result with
        | Any.Element(Element e) -> e
        | Any.Quote _ -> failwith "TODO"
        | Any.Network network -> networkToJs network
        | Any.Literal literal -> encodeString literal
        | Any.Variable (Variable variable) -> variable
        | Any.Pattern pattern -> failwith "TODO"
    | _ -> failwith "Error reading value."
