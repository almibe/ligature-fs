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

let patternNameToJS (p: PatternName) =
    let res = createEmpty

    match p with
    | PatternName.Name(Name(id)) -> res?identifier <- id
    | PatternName.Slot(Slot(Some(slot))) -> res?slot <- $"{slot}"
    | PatternName.Slot(Slot(None)) -> res?slot <- ""

    res


let rec networkToJS (network: Network) =
    let mutable resNetwork = [||]

    Set.iter
        (fun (e, a, v) ->
            let entity = patternNameToJS e
            let attribute = patternNameToJS a
            let value = valueToJS v

            resNetwork <- Array.append resNetwork [| [| entity; attribute; value |] |])
        network

    resNetwork

and quoteToJS (q: Quote) =
    List.map (fun value -> valueToJS (value)) q |> List.toArray


and valueToJS (v: LigatureValue) =
    let value = createEmpty

    match v with
    | LigatureValue.Bytes b -> failwith "TODO"
    | LigatureValue.Int i -> value?int <- i
    | LigatureValue.Quote q -> value?quote <- quoteToJS (q)
    | LigatureValue.Slot(Slot(Some(s))) -> value?slot <- s
    | LigatureValue.Slot(Slot(None)) -> value?slot <- ""
    | LigatureValue.String s -> value?string <- s
    | LigatureValue.Name(Name(i)) -> value?identifier <- i
    | LigatureValue.Network n -> value?network <- networkToJS n
    | LigatureValue.Expression(_) -> failwith "TODO"

    value

let partialResultToJS (result: LigatureValue option) =
    match result with
    | Some(value) -> valueToJS value
    | None -> createEmpty

//let stateToJS ((NetworkName(name), networks, partialResult): State) = failwith "TODO"
// let res = createEmpty
// let mutable resNetworks = []
// res?name <- name
// res?partialResult <- partialResultToJS partialResult

// Map.iter
//     (fun (NetworkName(name)) value ->
//         let networkRes = createEmpty
//         networkRes?name <- name
//         networkRes?network <- networkToJS value
//         resNetworks <- List.append resNetworks [ networkRes ])
//     networks

// res?networks <- List.toArray resNetworks
// res

let newEngine () =
    let wanderEngine: WanderEngine = new WanderEngine(stdCombinators, emptyStore)
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
