// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

open Ligature.Main
open Ligature.Wander.Lib.Combinators
open Ligature.Wander.Main
open Fable.Core.JsInterop

// let printNetwork (network: Network) : string =
//     Ligature.Wander.Model.printNetwork network

let networkToJS (network: Network) =
    let mutable resNetwork = [||]

    Set.iter
        (fun (e, a, v) ->
            let entity = createEmpty
            let attribute = createEmpty
            let value = createEmpty

            match e with
            | PatternName.Name(Name(id)) -> entity?identifier <- id
            | PatternName.Slot(Slot(Some(slot))) -> entity?slot <- slot
            | PatternName.Slot(Slot(None)) -> entity?slot <- ""

            match a with
            | PatternName.Name(Name(id)) -> attribute?identifier <- id
            | PatternName.Slot(Slot(Some(slot))) -> attribute?slot <- slot
            | PatternName.Slot(Slot(None)) -> attribute?slot <- ""

            match v with
            | LigatureValue.Bytes b -> failwith "TODO"
            | LigatureValue.Int i -> value?int <- i
            | LigatureValue.NetworkName n -> failwith "TODO"
            | LigatureValue.Quote q -> failwith "TODO"
            | LigatureValue.Slot(Slot(Some(s))) -> value?slot <- s
            | LigatureValue.Slot(Slot(None)) -> value?slot <- ""
            | LigatureValue.String s -> value?string <- s
            | LigatureValue.Name(Name(i)) -> value?identifier <- i

            resNetwork <- Array.append resNetwork [| [| entity; attribute; value |] |])
        network

    resNetwork

// let stateToJS ((NetworkName(name), networks): State) =
//     let mutable resNetworks = createEmpty


//     let res = createEmpty
//     res?network <- resNetwork
//     res

let stateToJS ((NetworkName(name), networks): State) =
    let res = createEmpty
    let mutable resNetworks = []
    res?name <- name

    Map.iter
        (fun (NetworkName(name)) value ->
            let networkRes = createEmpty
            networkRes?name <- name
            networkRes?network <- networkToJS value
            resNetworks <- List.append resNetworks [ networkRes ])
        networks

    res?networks <- List.toArray resNetworks
    res

let newEngine () =
    let engine = createEmpty
    let mutable state = defaultState
    let combinators = stdCombinators

    engine?run <-
        fun (script: string) ->
            match run combinators state script with
            | Ok res ->
                state <- res
                //Encode.Auto.toString(stateToJS res)
                stateToJS res
            | Error err -> failwith err.UserMessage

    engine
