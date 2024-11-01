﻿// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

open Ligature.Main
open Fable.Core.JsInterop
open Wander.Main
open Wander.Commands
open Ligature.InMemoryStore
open Wander.Model
open Wander.Lib

let printNetwork (network: Network) : string = Wander.Model.printNetwork network

let symbolToJS (Symbol(symbol): Symbol) =
    let res = createEmpty
    res?symbol <- symbol
    res

let rec storeToJS (store: LigatureStore) =
    let res = createEmpty

    Set.iter
        (fun network ->
            let networkInJS = store.Read network |> networkToJS
            res?(network) <- networkInJS)
        (store.Networks())

    res

and networkToJS (network: Network) =
    let mutable resNetwork = [||]

    Set.iter
        (fun (entry: Entry) ->
            match entry with
            | Entry.Extension e ->
                let res = createEmpty
                res?element <- symbolToJS e.element
                res?concept <- symbolToJS e.concept
                res?``type`` <- "extension"

                resNetwork <- Array.append resNetwork [| res |]
            | Entry.NonExtension ne ->
                let res = createEmpty
                res?element <- symbolToJS ne.element
                res?concept <- symbolToJS ne.concept
                res?``type`` <- "nonextension"

                resNetwork <- Array.append resNetwork [| res |]
            | Entry.Role role ->
                let res = createEmpty
                res?first <- symbolToJS role.first
                res?second <- symbolToJS role.second
                res?role <- symbolToJS role.role
                res?``type`` <- "role"

                resNetwork <- Array.append resNetwork [| res |])
        network

    resNetwork

let valueToJS (value: WanderValue) =
    let res = createEmpty

    match value with
    | WanderValue.Symbol(Symbol(id)) -> res?symbol <- id
    | WanderValue.Call e -> failwith "TODO"
    | WanderValue.Network n -> res?network <- (networkToJS n)

    res

let runScript (script: string) =
    let store = emptyInMemoryStore ()

    match run stdCommands store script with
    | Ok _ -> storeToJS store
    | _ -> failwith "TODO"

type WanderEngine(commands: Commands, store: LigatureStore) =
    member _.Run(script) = run commands store script
    member _.ReadStore() = store

let newEngine (wanderEngine: WanderEngine) =
    let engine = createEmpty

    engine?run <-
        fun (script: string) ->
            match wanderEngine.Run script with
            | Ok(Some(res)) -> valueToJS res
            | Ok _ -> createEmpty
            | Error err ->
                let res = createEmpty
                res?error <- err.UserMessage
                res

    engine?readStore <- fun () -> wanderEngine.ReadStore() |> storeToJS

    engine

let newInMemoryEngine () : WanderEngine =
    newEngine (new WanderEngine(stdCommands, emptyInMemoryStore ()))
