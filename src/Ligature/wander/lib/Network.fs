// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Network

open Ligature.Main
open Ligature.LigatureStore

let chompCombinator =
    { Name = Symbol("chomp")
      Doc = "Merge the passed Network into the current working Network."
      Signature = [ LigatureType.Network ], None
      Eval =
        fun _ networks (arguments: Arguments) ->
            match arguments with
            | [ WanderValue.Network(input) ] ->
                // let currentNetwork = currentNetwork networks
                // let newNetwork = Set.union input currentNetwork
                // let newNetworks = Map.add selected newNetwork networks
                // Ok(selected, newNetworks, None)
                failwith "TODO"
            | _ -> error "Bad call to chomp." None }

let unionCombinator =
    { Name = Symbol("union")
      Doc = "Find the union of two Networks."
      Signature = [ LigatureType.Network; LigatureType.Network ], Some LigatureType.Network
      Eval =
        fun _ networks (arguments: Arguments) ->
            match arguments with
            | [ WanderValue.Network(left); WanderValue.Network(right) ] ->
                let result = Set.union left right |> WanderValue.Network
                Ok(Some(result))
            | _ -> failwith "TODO" }

let minusCombinator =
    { Name = Symbol("minus")
      Doc = "Remove all Statements from the first Network that are in the second Networks."
      Signature = [ LigatureType.Network; LigatureType.Network ], Some LigatureType.Network
      Eval =
        fun _ networks (arguments: Arguments) ->
            match arguments with
            | [ WanderValue.Network(left); WanderValue.Network(right) ] ->
                let result = Set.difference left right |> WanderValue.Network
                Ok(Some(result))
            | _ -> failwith "TODO" }

let lookupNetwork () = failwith "TODO"

let applyNetwork (template: Network) (data: Network) : Network =
    Set.map
        (fun ((e, a, v)) ->
            let entity =
                match e with
                | Identifier.Slot s ->
                    match readBinding (Identifier.Slot s) data with
                    | Some(Identifier.Symbol(i)) -> Identifier.Symbol i
                    | Some(Identifier.Slot(s)) -> Identifier.Slot s
                    | Some _ -> failwith "TODO - unexpected value - only Slots or Symbols are allowed"
                    | None -> Identifier.Slot s
                | Identifier.Symbol i -> Identifier.Symbol i

            let attribute =
                match a with
                | Identifier.Slot s ->
                    match readBinding (Identifier.Slot s) data with
                    | Some(Identifier.Symbol(i)) -> Identifier.Symbol i
                    | Some(Identifier.Slot(s)) -> Identifier.Slot s
                    | Some _ -> failwith "TODO - unexpected value - only Slots or Symbols are allowed"
                    | None -> Identifier.Slot s
                | Identifier.Symbol i -> Identifier.Symbol i

            let value =
                match v with
                | Identifier.Slot s ->
                    match readBinding (Identifier.Slot s) data with
                    | Some value -> value
                    | None -> Identifier.Slot s
                | v -> v

            (entity, attribute, value))
        template


let applyCombinator: Combinator =
    { Name = Symbol("apply")
      Doc = ""
      Signature = [ LigatureType.Network; LigatureType.Quote ], Some LigatureType.Network
      Eval =
        fun combinators networks arguments ->
            match arguments with
            | [ WanderValue.Network(template); WanderValue.Quote(data) ] ->
                List.map
                    (fun dataset ->
                        match dataset with
                        | WanderValue.Network dataset -> applyNetwork template dataset |> WanderValue.Network
                        | _ -> failwith "TODO")
                    data
                |> WanderValue.Quote
                |> fun value -> Ok(Some(value))
            | _ -> failwith "TODO" }

let matchCombinator =
    { Name = Symbol("match")
      Doc = "args: pattern data, returns: quote of networks"
      Signature = [ LigatureType.Network; LigatureType.Network ], Some LigatureType.Quote
      Eval =
        fun _ networks arguments ->
            match arguments with
            | [ WanderValue.Network(pattern); WanderValue.Network(data) ] ->
                let res = matchNetwork pattern data
                Ok(Some(res))
            | _ -> error "" None }

let queryCombinator =
    { Name = Symbol("query")
      Doc = "arguments: pattern template data, returns network"
      Signature = [ LigatureType.Network; LigatureType.Network; LigatureType.Network ], Some LigatureType.Network
      Eval =
        fun combinators networks arguments ->
            let currentNetwork = currentNetwork networks

            match arguments with
            | [ WanderValue.Network pattern; WanderValue.Network template; WanderValue.Network data ] ->
                match
                    matchCombinator.Eval combinators networks [ WanderValue.Network pattern; WanderValue.Network data ]
                with
                | Ok(Some(WanderValue.Quote(res))) ->
                    applyCombinator.Eval combinators networks [ WanderValue.Network template; WanderValue.Quote res ]
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
