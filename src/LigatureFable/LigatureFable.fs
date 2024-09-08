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

let symbolToJS (Symbol(symbol): Symbol) =
    let res = createEmpty
    res?symbol <- symbol
    res

let valueToJS (value: WanderValue) =
    let res = createEmpty

    match value with
    | WanderValue.Symbol(Symbol(id)) -> res?identifier <- id
    | WanderValue.Slot(Slot(Some(slot))) -> res?slot <- $"{slot}"
    | WanderValue.Slot(Slot(None)) -> res?slot <- ""
    | WanderValue.Expression e -> failwith "TODO"
    | WanderValue.Network n -> failwith "TODO"
    | WanderValue.Quote q -> failwith "TODO"

    res

let rec networkToJS (network: Network) =
    let mutable resNetwork = [||]

    Set.iter
        (fun (e, a, v) ->
            let entity = symbolToJS e
            let attribute = symbolToJS a
            let value = symbolToJS v

            resNetwork <- Array.append resNetwork [| [| entity; attribute; value |] |])
        network

    resNetwork

// and quoteToJS (q: Quote) =
//     List.map (fun value -> valueToJS (value)) q |> List.toArray

// and valueToJS (v: Pattern) =
//     let value = createEmpty

//     match v with
//     | Identifier.Quote q -> value?quote <- quoteToJS (q)
//     | Pattern.Slot(Slot(Some(s))) -> value?slot <- s
//     | Pattern.Slot(Slot(None)) -> value?slot <- ""
//     | Pattern.Symbol(Symbol(i)) -> value?identifier <- i
//     | Identifier.Network n -> value?network <- networkToJS n
//     | Identifier.Expression(_) -> failwith "TODO"

//     value

// let partialResultToJS (result: Pattern option) =
//     match result with
//     | Some(value) -> valueToJS value
//     | None -> createEmpty

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

let newInMemoryEngine (): WanderEngine =
    newEngine (new WanderEngine(stdCombinators, emptyInMemoryStore))
