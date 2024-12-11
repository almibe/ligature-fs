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
                | Ok res -> networkToGraphology res
                | _ -> failwith "TODO"

            res?(network) <- networkInJS)
        (match store.Networks() with
         | Ok res -> res
         | Error _ -> failwith "TODO")

    res

and networkToGraphology (network: Network) =
    let res = createEmpty
    let mutable nodes = Set.empty
    let mutable edges = Set.empty

    Set.iter
        (fun (entry: Entry) ->
            match entry with
            | Entry.Extends { element = Element element
                              concept = Element concept } ->
                let elementJs = createEmpty
                elementJs?key <- element
                let conceptJs = createEmpty
                conceptJs?key <- concept
                nodes <- Set.add elementJs nodes
                nodes <- Set.add conceptJs nodes
                let edgeJs = createEmpty
                edgeJs?key <- ":"
                edgeJs?source <- element
                edgeJs?target <- concept
                edges <- Set.add edgeJs edges
            | Entry.NotExtends { element = Element element
                                 concept = Element concept } ->
                let elementJs = createEmpty
                elementJs?key <- element
                let conceptJs = createEmpty
                conceptJs?key <- concept
                nodes <- Set.add elementJs nodes
                nodes <- Set.add conceptJs nodes
                let edgeJs = createEmpty
                edgeJs?key <- "¬:"
                edgeJs?source <- element
                edgeJs?target <- concept
                edges <- Set.add edgeJs edges
            | Entry.Role { first = Element first
                           second = Element second
                           role = Element role } ->
                let firstJs = createEmpty
                firstJs?key <- first
                let secondJs = createEmpty
                secondJs?key <- second
                nodes <- Set.add firstJs nodes
                nodes <- Set.add secondJs nodes
                let edgeJs = createEmpty
                edgeJs?key <- role
                edgeJs?source <- first
                edgeJs?target <- second
                edges <- Set.add edgeJs edges
            | Entry.Attribute { element = Element element
                                attribute = Element attribute
                                value = Value value } ->
                let elementJs = createEmpty
                elementJs?key <- element
                let valueJs = createEmpty
                valueJs?key <- value
                nodes <- Set.add elementJs nodes
                nodes <- Set.add valueJs nodes
                let attributeJs = createEmpty
                attributeJs?key <- attribute
                attributeJs?source <- element
                attributeJs?target <- value
                edges <- Set.add attributeJs edges)
        network

    res?nodes <- Set.toArray nodes
    res?edges <- Set.toArray edges
    res

let runScript (script: string) =
    let store = newInMemoryEngine ()

    match run stdCommands store script with
    | Ok _ -> storeToJS store
    | _ -> failwith "TODO"

let readValue (input: string) =
    printfn $"In readValue: {input}"

    match read input with
    | Ok result ->
        match result with
        | WanderValue.Element(Element e) -> e
        | WanderValue.Call _ -> failwith "TODO - support writing calls"
        | WanderValue.Network network -> networkToGraphology network
    | _ -> failwith "Error reading value."
