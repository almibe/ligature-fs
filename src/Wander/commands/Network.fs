// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Commands.Network

open Ligature.Main
open Ligature.InMemoryStore
open Wander.Model

let chompCommand =
    { Name = Symbol("chomp")
      Doc = "Merge the passed Network into named Network."
      Signature = [ WanderType.Network; WanderType.Symbol ], None
      Eval =
        fun _ networks (arguments: Arguments) ->
            match arguments with
            | [ WanderValue.Network(input); WanderValue.Symbol(name) ] ->
                // let currentNetwork = currentNetwork networks
                // let newNetwork = Set.union input currentNetwork
                // let newNetworks = Map.add selected newNetwork networks
                // Ok(selected, newNetworks, None)
                failwith "TODO"
            | _ -> error "Bad call to chomp." None }

let isConsistentCommand =
    { Name = Symbol("is-consistent?")
      Doc = "Determine if a Network is consistent."
      Signature = [ WanderType.Network; WanderType.Network ], Some WanderType.Network
      Eval =
        fun _ store (arguments: Arguments) ->
            match arguments with
            | [ WanderValue.Symbol(Symbol(networkName)) ] ->
                let value = store.IsConsistent networkName
                Ok(Some(WanderValue.Symbol(value.ToString().ToLower() |> Symbol)))
            | [ WanderValue.Network(network) ] ->
                match isConsistent network with
                | value -> Ok(Some(WanderValue.Symbol(value.ToString().ToLower() |> Symbol)))
            //| Error err -> error "Bad call to is-consistent." None
            | _ -> error "Bad call to is-consistent." None }

let isCompleteCommand =
    { Name = Symbol("is-complete?")
      Doc = "Determine if a Network is complete."
      Signature = [ WanderType.Network ], Some WanderType.Network
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

let unionCommand =
    { Name = Symbol("union")
      Doc = "Find the union of two Networks."
      Signature = [ WanderType.Network; WanderType.Network ], Some WanderType.Network
      Eval =
        fun _ networks (arguments: Arguments) ->
            match arguments with
            | [ WanderValue.Network(left); WanderValue.Network(right) ] ->
                let result = Set.union left right |> WanderValue.Network
                Ok(Some(result))
            | _ -> failwith "TODO" }

let countCommand =
    { Name = Symbol("count")
      Doc = "Count the number of assertions in a Network."
      Signature = [ WanderType.Symbol ], Some WanderType.Symbol
      Eval =
        fun _ store (arguments: Arguments) ->
            match arguments with
            | [ WanderValue.Symbol(Symbol name) ] ->
                match store.Read name with
                | network -> Ok(Some(WanderValue.Symbol(Symbol(network.Count.ToString()))))
            | [ WanderValue.Network network ] -> Ok(Some(WanderValue.Symbol(Symbol(network.Count.ToString()))))
            | _ -> failwith "TODO" }

let minusCommand =
    { Name = Symbol("minus")
      Doc = "Remove all Statements from the first Network that are in the second Networks."
      Signature = [ WanderType.Network; WanderType.Network ], Some WanderType.Network
      Eval =
        fun _ networks (arguments: Arguments) ->
            match arguments with
            | [ WanderValue.Network(left); WanderValue.Network(right) ] ->
                let result = Set.difference left right |> WanderValue.Network
                Ok(Some(result))
            | _ -> failwith "TODO" }

// let applyNetwork (template: Network) (data: Network) : Network =
//     Set.map
//         (fun entry ->
//             match entry with
//             | Extension extension -> failwith "TODO"
//             | NonExtension nonExtension -> failwith "TODO"
//             | Role role -> failwith "TODO"
//             // let entity =
//             //     match e with
//             //     | Pattern.Slot s ->
//             //         match readBinding (Pattern.Slot s) data with
//             //         | Some(Pattern.Symbol(i)) -> Pattern.Symbol i
//             //         | Some(Pattern.Slot(s)) -> Pattern.Slot s
//             //         | Some _ -> failwith "TODO - unexpected value - only Slots or Symbols are allowed"
//             //         | None -> Pattern.Slot s
//             //     | Pattern.Symbol i -> Pattern.Symbol i

//             // let attribute =
//             //     match a with
//             //     | Pattern.Slot s ->
//             //         match readBinding (Pattern.Slot s) data with
//             //         | Some(Pattern.Symbol(i)) -> Pattern.Symbol i
//             //         | Some(Pattern.Slot(s)) -> Pattern.Slot s
//             //         | Some _ -> failwith "TODO - unexpected value - only Slots or Symbols are allowed"
//             //         | None -> Pattern.Slot s
//             //     | Pattern.Symbol i -> Pattern.Symbol i

//             // let value =
//             //     match v with
//             //     | Pattern.Slot s ->
//             //         match readBinding (Pattern.Slot s) data with
//             //         | Some value -> value
//             //         | None -> Pattern.Slot s
//             //     | v -> v

//             // (entity, attribute, value))
//             )
//         template

let queryCommand =
    { Name = Symbol("query")
      Doc = "arguments: pattern template data, returns network"
      Signature = [ WanderType.Network; WanderType.Network; WanderType.Network ], Some WanderType.Network
      Eval =
        fun commands networks arguments ->
            match arguments with
            | [ WanderValue.Network pattern; WanderValue.Network template; WanderValue.Network source ] ->
                failwith "TODO"
            | _ -> error "Invalid call to query" None }

let networkCommands: Map<Symbol, Command> =
    (Map.ofList
        [ (chompCommand.Name, chompCommand)
          (countCommand.Name, countCommand)
          (minusCommand.Name, minusCommand)
          (queryCommand.Name, queryCommand)
          (unionCommand.Name, unionCommand)
          (isCompleteCommand.Name, isCompleteCommand)
          (isConsistentCommand.Name, isConsistentCommand) ])
