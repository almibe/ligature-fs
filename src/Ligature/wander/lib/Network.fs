// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Network

open Ligature.Main
open Ligature.InMemoryNetwork
open Ligature.Wander.Interpreter
open FSharpPlus

let chompCombinator =
    { Name = Name("chomp")
      Doc = ""
      Signature = []
      Eval =
        fun _ selected networks (arguments: Arguments) ->
            match arguments with
            | [ LigatureValue.Network(input) ] ->
                let currentNetwork = currentNetwork selected networks
                let newNetwork = Set.union input currentNetwork
                let newNetworks = Map.add selected newNetwork networks
                Ok(selected, newNetworks, None)
            | _ -> error "Bad call to chomp." None }

let unionCombinator =
    { Name = Name("union")
      Doc = "Find the union of two Networks."
      Signature = []
      Eval =
        fun _ networkName networks (arguments: Arguments) ->
            match arguments with
            | [ LigatureValue.Network(left); LigatureValue.Network(right) ] ->
                let result = Set.union left right |> LigatureValue.Network
                Ok(networkName, networks, Some(result))
            | _ -> failwith "TODO" }

let minusCombinator =
    { Name = Name("minus")
      Doc = "Remove all Statements from the first Network that are in the second Networks."
      Signature = []
      Eval =
        fun _ networkName networks (arguments: Arguments) ->
            match arguments with
            | [ LigatureValue.Network(left); LigatureValue.Network(right) ] ->
                let result = Set.difference left right |> LigatureValue.Network
                Ok(networkName, networks, Some(result))
            | _ -> failwith "TODO" }

let lookupNetwork () = failwith "TODO"

let applyNetwork (template: Network) (data: Network) : Network =
    Set.map
        (fun ((e, a, v)) ->
            let entity =
                match e with
                | PatternName.Slot s ->
                    match readBinding (PatternName.Slot s) data with
                    | Some(LigatureValue.Name(i)) -> PatternName.Name i
                    | Some(LigatureValue.Slot(s)) -> PatternName.Slot s
                    | Some _ -> failwith "TODO - unexpected value - only Slots or Names are allowed"
                    | None -> PatternName.Slot s
                | PatternName.Name i -> PatternName.Name i

            let attribute =
                match a with
                | PatternName.Slot s ->
                    match readBinding (PatternName.Slot s) data with
                    | Some(LigatureValue.Name(i)) -> PatternName.Name i
                    | Some(LigatureValue.Slot(s)) -> PatternName.Slot s
                    | Some _ -> failwith "TODO - unexpected value - only Slots or Names are allowed"
                    | None -> PatternName.Slot s
                | PatternName.Name i -> PatternName.Name i

            let value =
                match v with
                | LigatureValue.Slot s ->
                    match readBinding (PatternName.Slot s) data with
                    | Some value -> value
                    | None -> LigatureValue.Slot s
                | v -> v

            (entity, attribute, value))
        template


let applyCombinator: Combinator =
    { Name = Name("apply")
      Doc = ""
      Signature = []
      Eval =
        fun combinators networkName networks arguments ->
            match arguments with
            | [ LigatureValue.Network(template); LigatureValue.Quote(data) ] ->
                List.map
                    (fun dataset ->
                        match dataset with
                        | LigatureValue.Network dataset -> applyNetwork template dataset |> LigatureValue.Network
                        | _ -> failwith "TODO")
                    data
                |> LigatureValue.Quote
                |> fun value -> Ok(networkName, networks, Some(value))
            | _ -> failwith "TODO" }

let matchCombinator =
    { Name = Name("match")
      Doc = "args: pattern data\nreturns: quote of networks"
      Signature = []
      Eval =
        fun _ networkName networks arguments ->
            let currentNetwork = currentNetwork networkName networks

            match arguments with
            | [ LigatureValue.Network(pattern); LigatureValue.Network(data) ] ->
                let res = matchNetwork pattern data
                Ok(networkName, networks, Some(res))
            | _ -> error "" None }

let queryCombinator =
    { Name = Name("query")
      Doc = ""
      Signature = []
      Eval =
        fun combinators networkName networks arguments ->
            let currentNetwork = currentNetwork networkName networks

            match arguments with
            | [ LigatureValue.Network pattern; LigatureValue.Network template; LigatureValue.Network data ] ->
                match
                    matchCombinator.Eval
                        combinators
                        networkName
                        networks
                        [ LigatureValue.Network pattern; LigatureValue.Network data ]
                with
                | Ok(_, _, Some(LigatureValue.Quote(res))) ->
                    applyCombinator.Eval
                        combinators
                        networkName
                        networks
                        [ LigatureValue.Network template; LigatureValue.Quote res ]
                | _ -> failwith "TODO"
            | _ -> failwith "TODO" }

let networkCombinators =
    (Map.ofList
        [ (applyCombinator.Name, applyCombinator)
          (chompCombinator.Name, chompCombinator)
          (matchCombinator.Name, matchCombinator)
          (minusCombinator.Name, minusCombinator)
          (queryCombinator.Name, queryCombinator)
          (unionCombinator.Name, unionCombinator) ])
