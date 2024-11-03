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

let applySingle (template: Network) (data: Map<Symbol, Symbol>) : Network =
    Set.map
        (fun entry ->
            match entry with
            | Entry.Role { first = Symbol first
                           second = Symbol second
                           role = Symbol role } ->
                let resFirst =
                    if first.StartsWith "?" then
                        match Map.tryFind (Symbol first) data with
                        | Some res -> res
                        | _ -> Symbol first
                    else
                        Symbol first

                let resSecond =
                    if second.StartsWith "?" then
                        match Map.tryFind (Symbol second) data with
                        | Some res -> res
                        | _ -> Symbol second
                    else
                        Symbol second

                let resRole =
                    if role.StartsWith "?" then
                        match Map.tryFind (Symbol role) data with
                        | Some res -> res
                        | _ -> Symbol role
                    else
                        Symbol role

                Entry.Role
                    { first = resFirst
                      second = resSecond
                      role = resRole }
            | Entry.Extension _ -> failwith "TODO"
            | Entry.NonExtension _ -> failwith "TODO")
        template

let apply (template: Network) (data: Set<Map<Symbol, Symbol>>) : Network =
    Set.fold (fun state value -> Set.union state (applySingle template value)) Set.empty data

let compareRole
    ({ first = Symbol pFirst
       second = Symbol pSecond
       role = Symbol pRole }: Role)
    ({ first = Symbol sFirst
       second = Symbol sSecond
       role = Symbol sRole }: Role)
    : Set<Map<Symbol, Symbol>> =
    let mutable fail = false
    let mutable result = Map.empty

    if pFirst.StartsWith "?" then
        if pFirst.Length > 1 then
            result <- Map.add (Symbol pFirst) (Symbol sFirst) result
    else
        fail <- pFirst <> sFirst

    if (not fail) && pSecond.StartsWith "?" then
        if pSecond.Length > 1 then
            result <- Map.add (Symbol pSecond) (Symbol sSecond) result
    else
        fail <- pSecond <> sSecond

    if (not fail) && pRole.StartsWith "?" then
        if pRole.Length > 1 then
            result <- Map.add (Symbol pRole) (Symbol sRole) result
    else
        fail <- pRole <> sRole

    if fail || result = Map.empty then
        Set.empty
    else
        Set.ofList [ result ]

let compareExtension (pattern: Extension) (source: Extension) = failwith "TODO"

let compareNonExtension (pattern: NonExtension) (source: NonExtension) = failwith "TODO"

let findSingleEntry (pattern: Entry) (source: Entry) : Set<Map<Symbol, Symbol>> =
    match pattern, source with
    | Entry.Role pattern, Entry.Role source -> compareRole pattern source
    | Entry.Extension pattern, Entry.Extension source -> compareExtension pattern source
    | Entry.NonExtension pattern, Entry.NonExtension source -> compareNonExtension pattern source
    | _ -> Set.empty

let findEntry (pattern: Entry) (source: Network) : Set<Map<Symbol, Symbol>> =
    Set.fold (fun state entry -> Set.union state (findSingleEntry pattern entry)) Set.empty source

let find (pattern: Network) (source: Network) : Set<Map<Symbol, Symbol>> =
    Set.fold (fun state part -> Set.union state (findEntry part source)) Set.empty pattern

let queryCommand =
    { Name = Symbol("query")
      Doc = "arguments: pattern template data, returns network"
      Signature = [ WanderType.Network; WanderType.Network; WanderType.Network ], Some WanderType.Network
      Eval =
        fun commands networks arguments ->
            match arguments with
            | [ WanderValue.Network pattern; WanderValue.Network template; WanderValue.Network source ] ->
                let results = find pattern source
                Ok(Some(WanderValue.Network(apply template results)))
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
