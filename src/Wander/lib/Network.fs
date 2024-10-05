// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Lib.Network

open Ligature.Main
open Ligature.InMemoryStore

let chompCombinator =
    { Name = Symbol("chomp")
      Doc = "Merge the passed Network into named Network."
      Signature = [ LigatureType.Network; LigatureType.Symbol ], None
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

let isConsistentCombinator =
    { Name = Symbol("is-consistent?")
      Doc = "Determine if a Network is consistent."
      Signature = [ LigatureType.Network; LigatureType.Network ], Some LigatureType.Network
      Eval =
        fun _ store (arguments: Arguments) ->
            match arguments with
            | [ WanderValue.Symbol(Symbol(networkName)) ] ->
                let value = store.IsConsistent networkName
                Ok(Some(WanderValue.Symbol(value.ToString().ToLower() |> Symbol)))
            | [ WanderValue.Network(network) ] ->
                match consistent network with
                | value -> Ok(Some(WanderValue.Symbol(value.ToString().ToLower() |> Symbol)))
            //| Error err -> error "Bad call to is-consistent." None
            | _ -> error "Bad call to is-consistent." None }

let isCompleteCombinator =
    { Name = Symbol("is-complete?")
      Doc = "Determine if a Network is complete."
      Signature = [ LigatureType.Network ], Some LigatureType.Network
      Eval =
        fun _ store (arguments: Arguments) ->
            match arguments with
            | [ WanderValue.Symbol(Symbol(networkName)) ] ->
                let value = store.IsComplete networkName
                Ok(Some(WanderValue.Symbol(value.ToString().ToLower() |> Symbol)))
            | [ WanderValue.Network(network) ] ->
                let value = isComplete network
                Ok(Some(WanderValue.Symbol(value.ToString().ToLower() |> Symbol)))
            | _ -> error "Bad call to is-complete." None }

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

let countCombinator =
    { Name = Symbol("count")
      Doc = "Count the number of assertions in a Network."
      Signature = [ LigatureType.Symbol ], Some LigatureType.Symbol
      Eval =
        fun _ store (arguments: Arguments) ->
            match arguments with
            | [ WanderValue.Symbol(Symbol name) ] ->
                match store.Read name with
                | network -> Ok(Some(WanderValue.Symbol(Symbol(network.Count.ToString()))))
            | [ WanderValue.Network network ] -> Ok(Some(WanderValue.Symbol(Symbol(network.Count.ToString()))))
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

// let applyNetwork (template: Network) (data: Network) : Network =
// Set.map
//     (fun ((e, a, v)) ->
//         let entity =
//             match e with
//             | Pattern.Slot s ->
//                 match readBinding (Pattern.Slot s) data with
//                 | Some(Pattern.Symbol(i)) -> Pattern.Symbol i
//                 | Some(Pattern.Slot(s)) -> Pattern.Slot s
//                 | Some _ -> failwith "TODO - unexpected value - only Slots or Symbols are allowed"
//                 | None -> Pattern.Slot s
//             | Pattern.Symbol i -> Pattern.Symbol i

//         let attribute =
//             match a with
//             | Pattern.Slot s ->
//                 match readBinding (Pattern.Slot s) data with
//                 | Some(Pattern.Symbol(i)) -> Pattern.Symbol i
//                 | Some(Pattern.Slot(s)) -> Pattern.Slot s
//                 | Some _ -> failwith "TODO - unexpected value - only Slots or Symbols are allowed"
//                 | None -> Pattern.Slot s
//             | Pattern.Symbol i -> Pattern.Symbol i

//         let value =
//             match v with
//             | Pattern.Slot s ->
//                 match readBinding (Pattern.Slot s) data with
//                 | Some value -> value
//                 | None -> Pattern.Slot s
//             | v -> v

//         (entity, attribute, value))
//     template

// let applyCombinator: Combinator =
//     { Name = Symbol("apply")
//       Doc = ""
//       Signature = [ LigatureType.Network; LigatureType.Quote ], Some LigatureType.Network
//       Eval =
//         fun combinators networks arguments ->
//             match arguments with
//             | [ WanderValue.Network(template); WanderValue.Quote(data) ] ->
//                 List.map
//                     (fun dataset ->
//                         match dataset with
//                         | WanderValue.Network dataset -> applyNetwork template dataset |> WanderValue.Network
//                         | value -> failwith $"TODO -- {value}")
//                     data
//                 |> WanderValue.Quote
//                 |> fun value -> Ok(Some(value))
//             | _ -> failwith "TODO" }

// let matchCombinator =
//     { Name = Symbol("match")
//       Doc = "args: pattern data, returns: quote of networks"
//       Signature = [ LigatureType.Network; LigatureType.Network ], Some LigatureType.Quote
//       Eval =
//         fun _ networks arguments ->
//             match arguments with
//             | [ WanderValue.Network(pattern); WanderValue.Network(data) ] ->
//                 let res = matchNetwork pattern data
//                 Ok(Some(res))
//             | _ -> error "" None }

// let queryCombinator =
//     { Name = Symbol("query")
//       Doc = "arguments: pattern template data, returns network"
//       Signature = [ LigatureType.Network; LigatureType.Network; LigatureType.Network ], Some LigatureType.Network
//       Eval =
//         fun combinators networks arguments ->
//             let currentNetwork = currentNetwork networks

//             match arguments with
//             | [ WanderValue.Network pattern; WanderValue.Network template; WanderValue.Network data ] ->
//                 match
//                     matchCombinator.Eval combinators networks [ WanderValue.Network pattern; WanderValue.Network data ]
//                 with
//                 | Ok(Some(WanderValue.Quote(res))) ->
//                     applyCombinator.Eval combinators networks [ WanderValue.Network template; WanderValue.Quote res ]
//                 | _ -> failwith "TODO"
//             | _ -> failwith "TODO" }

let networkCombinators: Map<Element, Combinator> =
    (Map.ofList
        [ //(applyCombinator.Name, applyCombinator)
          (chompCombinator.Name, chompCombinator)
          (countCombinator.Name, countCombinator)
          //          (matchCombinator.Name, matchCombinator)
          (minusCombinator.Name, minusCombinator)
          //          (queryCombinator.Name, queryCombinator)
          (unionCombinator.Name, unionCombinator)
          (isCompleteCombinator.Name, isCompleteCombinator)
          (isConsistentCombinator.Name, isConsistentCombinator) ])
