// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

open Ligature.Main
open Fable.Core.JsInterop
open Wander.Main
open Wander.Commands
open Ligature.InMemoryEngine
open Wander.Lib
open Wander.Model

let rec storeToJS (store: LigatureEngine) =
    let res = createEmpty

    Set.iter
        (fun network ->
            let networkInJS =
                match store.ReadNetwork network with
                | Ok res -> networkToJs res
                | _ -> failwith "TODO"

            res?(network) <- networkInJS)
        (match store.Networks() with
         | Ok res -> res
         | Error _ -> failwith "TODO")

    res

and networkToJs (network: Network) =
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
            | Entry.Role { first = Element first
                           second = Element second
                           role = Element role } ->
                let elementJs = createEmpty
                elementJs?first <- first
                elementJs?second <- role
                elementJs?third <- second
                entries <- Set.add elementJs entries
            | Entry.Attribute { element = Element element
                                attribute = Element attribute
                                value = Value value } ->
                let elementJs = createEmpty
                elementJs?first <- element
                elementJs?second <- attribute
                elementJs?third <- encodeString value
                entries <- Set.add elementJs entries)
        network

    res?entries <- Set.toArray entries
    res

let runScript (script: string) =
    let store = newInMemoryEngine ()

    match run stdCommands store script with
    | Ok _ -> storeToJS store
    | _ -> failwith "TODO"

let readValue (input: string) =
    match read input with
    | Ok result ->
        match result with
        | WanderValue.Element(Element e) -> e
        | WanderValue.Call _ -> failwith "TODO - support writing calls"
        | WanderValue.Network network -> networkToJs network
    | _ -> failwith "Error reading value."
