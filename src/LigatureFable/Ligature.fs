// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

open Ligature.Main
open Ligature.Wander.Lib.Combinators
open Ligature.Wander.Main
open Fable.Core.JsInterop

let printResult (script: string) : string = run stdState script |> printResult

let stateToJS (networkName, networks) =
    let currentNetwork = currentNetwork (networkName, networks)
    let mutable resNetwork = [||]

    Set.iter
        (fun (e, a, v) ->
            let entity = createEmpty
            let attribute = createEmpty
            let value = createEmpty

            match e with
            | PatternIdentifier.Identifier(Identifier(id)) -> entity?identifier <- id
            | PatternIdentifier.Slot(Slot(Some(slot))) -> entity?slot <- slot
            | PatternIdentifier.Slot(Slot(None)) -> entity?slot <- ""

            match a with
            | PatternIdentifier.Identifier(Identifier(id)) -> attribute?identifier <- id
            | PatternIdentifier.Slot(Slot(Some(slot))) -> attribute?slot <- slot
            | PatternIdentifier.Slot(Slot(None)) -> attribute?slot <- ""

            match v with
            | LigatureValue.Bytes b -> failwith "TODO"
            | LigatureValue.Int i -> value?int <- i
            | LigatureValue.Network n -> failwith "TODO"
            | LigatureValue.NetworkName n -> failwith "TODO"
            | LigatureValue.HostCombinator hc -> failwith "TODO"
            | LigatureValue.Quote q -> failwith "TODO"
            | LigatureValue.Slot(Slot(Some(s))) -> value?slot <- s
            | LigatureValue.Slot(Slot(None)) -> value?slot <- ""
            | LigatureValue.String s -> value?string <- s
            | LigatureValue.Identifier(Identifier(i)) -> value?identifier <- i

            resNetwork <- Array.append resNetwork [| [| entity; attribute; value |] |])
        currentNetwork

    let res = createEmpty
    res?name <- readNetworkName networkName
    res?network <- resNetwork
    res

let newEngine () =
    let engine = createEmpty
    let mutable state = defaultState

    engine?run <-
        fun (script: string) ->
            match run state script with
            | Ok res ->
                state <- res
                stateToJS res
            | Error err -> failwith err.UserMessage

    engine
