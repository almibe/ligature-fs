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

// let rec storeToJS (store: LigatureEngine) =
//     let res = createEmpty

//     Set.iter
//         (fun network ->
//             let networkInJS =
//                 match store.ReadNetwork network with
//                 | Ok res -> networkToJs res
//                 | _ -> failwith "TODO"

//             res?(network) <- networkInJS)
//         (match store.Networks() with
//          | Ok res -> res
//          | Error _ -> failwith "TODO")

//     res

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
                    | Value.Network network -> failwith "TODO"
                    | Value.Quote quote -> failwith "TODO"
                    | Value.Variable variable -> failwith "TODO"

                entries <- Set.add elementJs entries)
        network

    res?entries <- Set.toArray entries
    res

// let runScript (script: string) =
//     match run stdCommands (emptyVariables()) script with
//     | Ok _ -> storeToJS store
//     | _ -> failwith "TODO"

let readValue (input: string) =
    match read input with
    | Ok result ->
        match result with
        | Value.Element(Element e) -> e
        | Value.Quote _ -> failwith "TODO - support writing calls"
        | Value.Network network -> networkToJs network
        | Value.Literal(_) -> failwith "Not Implemented"
        | Value.Variable(_) -> failwith "Not Implemented"
    | _ -> failwith "Error reading value."
