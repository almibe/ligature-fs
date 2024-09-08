// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

open Ligature.Main
open Ligature.Wander.Combinators
open Ligature.Wander.Main
open Fable.Core.JsInterop
open Ligature.LigatureStore.InMemoryStore
open System.Collections.Generic

let printNetwork (network: Network) : string =
    Ligature.Wander.Model.printNetwork network

let patternToJS (p: Identifier) =
    let res = createEmpty

    match p with
    | Identifier.Symbol(Symbol(id)) -> res?identifier <- id
    | Identifier.Slot(Slot(Some(slot))) -> res?slot <- $"{slot}"
    | Identifier.Slot(Slot(None)) -> res?slot <- ""

    res


let rec networkToJS (network: Network) =
    let mutable resNetwork = [||]

    Set.iter
        (fun (e, a, v) ->
            let entity = patternToJS e
            let attribute = patternToJS a
            let value = valueToJS v

            resNetwork <- Array.append resNetwork [| [| entity; attribute; value |] |])
        network

    resNetwork

and quoteToJS (q: Quote) =
    List.map (fun value -> valueToJS (value)) q |> List.toArray

and valueToJS (v: Identifier) =
    let value = createEmpty

    match v with
    | Identifier.Quote q -> value?quote <- quoteToJS (q)
    | Identifier.Slot(Slot(Some(s))) -> value?slot <- s
    | Identifier.Slot(Slot(None)) -> value?slot <- ""
    | Identifier.Symbol(Symbol(i)) -> value?identifier <- i
    | Identifier.Network n -> value?network <- networkToJS n
    | Identifier.Expression(_) -> failwith "TODO"

    value

let partialResultToJS (result: Identifier option) =
    match result with
    | Some(value) -> valueToJS value
    | None -> createEmpty

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

    engine

let newIndexeddbEngine () = failwith "TODO"

let newInMemoryEngine () =
    newEngine (new WanderEngine(stdCombinators, emptyInMemoryStore))
