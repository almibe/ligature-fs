// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Commands.Network

open Ligature.Model
open Wander.Model
open Ligature.Core
open Wander.Interpreter

let chompCommand =
    { Name = Element("chomp")
      Doc = "Merge the passed Network into named Network."
      Eval =
        fun _ networks (arguments: Arguments) ->
            match arguments with
            | [ Any.Network(input); Any.Element(name) ] ->
                // let currentNetwork = currentNetwork networks
                // let newNetwork = Set.union input currentNetwork
                // let newNetworks = Map.add selected newNetwork networks
                // Ok(selected, newNetworks, None)
                failwith "TODO"
            | _ -> error "Bad call to chomp." None }

let isConsistentCommand =
    { Name = Element("is-consistent")
      Doc = "Determine if a Network is consistent."
      Eval =
        fun _ store (arguments: Arguments) ->
            match arguments with
            // | [ Value.Element(Element(networkName)) ] ->
            //     let value = store.IsConsistent networkName
            //     Ok(Some(Value.Element(value.ToString().ToLower() |> Element)))
            // | [ Value.Network(network) ] ->
            //     match isConsistent network with
            //     | value -> Ok(Some(Value.Element(value.ToString().ToLower() |> Element)))
            //| Error err -> error "Bad call to is-consistent." None
            | _ -> error "Bad call to is-consistent." None }

let isCompleteCommand =
    { Name = Element("is-complete")
      Doc = "Determine if a Network is complete."
      Eval =
        fun _ store (arguments: Arguments) ->
            match arguments with
            // | [ Value.Element(Element(networkName)) ] ->
            //     let value = store.IsComplete networkName
            //     Ok(Some(Value.Element(value.ToString().ToLower() |> Element)))
            // | [ Value.Network(network) ] ->
            //     let value = isComplete network
            //     Ok(Some(Value.Element(value.ToString().ToLower() |> Element)))
            | _ -> error "Bad call to is-complete." None }

let unionCommand =
    { Name = Element("union")
      Doc = "Find the union of two Networks."
      Eval =
        fun commands variables (arguments: Arguments) ->
            match arguments with
            | [ left; right ] ->
                let left =
                    match left with
                    | Any.Network n -> n
                    | Any.Variable v ->
                        match Map.tryFind v variables with
                        | Some(Any.Network res) -> res
                        | _ -> failwith "TODO"
                    | _ -> failwith "TODO"

                let right =
                    match right with
                    | Any.Network n -> n
                    | Any.Variable v ->
                        match Map.tryFind v variables with
                        | Some(Any.Network res) -> res
                        | _ -> failwith "TODO"
                    | Any.Quote quote ->
                        match evalQuote commands variables quote with
                        | Ok((Some(Any.Network network), _, _)) -> network
                        | _ -> failwith "TODO"
                    | _ -> failwith "TODO"

                let result = Set.union left right |> Any.Network
                Ok((Some(result), commands, variables))
            | args -> failwith $"TODO - {args}" }

let countCommand =
    { Name = Element("count")
      Doc = "Count the number of assertions in a Network."
      Eval =
        fun commands variables (arguments: Arguments) ->
            match arguments with
            | [ Any.Variable variable ] ->
                match variables.TryFind variable with
                | Some(Any.Network network) ->
                    Ok((Some(Any.Element(Element((Set.count network).ToString()))), commands, variables))
                | _ -> failwith "TODO"
            | [ Any.Network network ] -> Ok((Some(Any.Element(Element(network.Count.ToString()))), commands, variables))
            | [ Any.Quote quote ] ->
                match evalQuote commands variables quote with
                | Ok((Some(Any.Network network), commands, variables)) ->
                    Ok((Some(Any.Element(Element(network.Count.ToString()))), commands, variables))
                | _ -> failwith "TODO"
            | args -> failwith $"TODO - {args}" }

let minusCommand =
    { Name = Element("minus")
      Doc = "Remove all Statements from the first Network that are in the second Networks."
      Eval =
        fun commands variables (arguments: Arguments) ->
            match arguments with
            | [ Any.Network(left); Any.Network(right) ] ->
                let result = Set.difference left right |> Any.Network
                Ok((Some(result), commands, variables))
            | _ -> failwith "TODO" }

let queryCommand =
    { Name = Element("query")
      Doc = "arguments: pattern template data, returns network"
      Eval =
        fun commands variables arguments ->
            match arguments with
            | [ pattern; template; source ] ->
                let pattern =
                    match pattern with
                    | Any.Network n -> n
                    | Any.Variable v ->
                        if variables.ContainsKey v then
                            match variables[v] with
                            | Any.Network n -> n
                            | _ -> failwith "TODO"
                        else
                            failwith "TODO"
                    | _ -> failwith "TODO"

                let template =
                    match template with
                    | Any.Network n -> n
                    | Any.Variable v ->
                        if variables.ContainsKey v then
                            match variables[v] with
                            | Any.Network n -> n
                            | _ -> failwith "TODO"
                        else
                            failwith "TODO"
                    | _ -> failwith "TODO"

                let source =
                    match source with
                    | Any.Network n -> n
                    | Any.Variable v ->
                        if variables.ContainsKey v then
                            match variables[v] with
                            | Any.Network n -> n
                            | _ -> failwith "TODO"
                        else
                            failwith "TODO"
                    | Any.Quote quote ->
                        match evalQuote commands variables quote with
                        | Ok((Some(Any.Network n), commands, variables)) -> n
                        | _ -> failwith "TODO"
                    | _ -> failwith "TODO"

                let results = query pattern template source
                Ok((Some(Any.Network results), commands, variables))
            | _ -> error "Invalid call to query" None }

let matchCommand =
    { Name = Element "match"
      Doc = "Match a pattern against a network."
      Eval =
        fun commands variables arguments ->
            match arguments with
            | [ Any.Quote [ e; a; v ]; Any.Network network ] ->
                let element =
                    match e with
                    | Any.Element e -> ElementPattern.Element e
                    | Any.Variable v -> ElementPattern.Variable v
                    | _ -> failwith "TODO"

                let attribute =
                    match a with
                    | Any.Element e -> ElementPattern.Element e
                    | Any.Variable v -> ElementPattern.Variable v
                    | _ -> failwith "TODO"

                let value =
                    match v with
                    | Any.Element e -> Value.Element e
                    | Any.Variable v -> Value.Variable v
                    | Any.Literal l -> Value.Literal l
                    | _ -> failwith "TODO"

                Ok((Some(Any.ResultSet(singleMatch (element, attribute, value) network)), commands, variables))
            | [ pattern; network ] ->
                let pattern =
                    match pattern with
                    | Any.Network n -> n
                    | Any.Quote q ->
                        match evalQuote commands variables q with
                        | Ok((Some(Any.Network n), commands, variables)) -> n
                        | _ -> failwith "TODO"
                    | _ -> failwith "TODO"

                let network =
                    match network with
                    | Any.Network n -> n
                    | Any.Quote q ->
                        match evalQuote commands variables q with
                        | Ok((Some(Any.Network n), commands, variables)) -> n
                        | _ -> failwith "TODO"
                    | _ -> failwith "TODO"

                Ok(Some(Any.ResultSet(networkMatch pattern network)), commands, variables)

            | _ -> failwith "TODO" }

let applyCommand =
    { Name = Element "apply"
      Doc = "Fill in Variables in a Network with values from a Result Set."
      Eval =
        fun commands variables arguments ->
            match arguments with
            | [ Any.Network network; Any.Quote q ] ->
                let resultSet =
                    match evalQuote commands variables q with
                    | Ok((Some(Any.ResultSet res), commands, variables)) -> res
                    | Ok _ -> failwith "TODO"
                    | Error err -> failwith "TODO"

                let res = apply network resultSet
                Ok((Some(Any.Network res), commands, variables))
            | _ -> failwith "TODO" }

let filterCommand =
    { Name = Element("filter")
      Doc = "arguments: pattern data, returns network"
      Eval =
        fun commands variables arguments ->
            match arguments with
            | [ pattern; source ] ->
                let pattern =
                    match pattern with
                    | Any.Network n -> n
                    | Any.Variable v ->
                        if variables.ContainsKey v then
                            match variables[v] with
                            | Any.Network n -> n
                            | _ -> failwith "TODO"
                        else
                            failwith "TODO"
                    | Any.Quote quote ->
                        match evalQuote commands variables quote with
                        | Ok((Some(Any.Network n), commands, variables)) -> n
                        | _ -> failwith "TODO"
                    | _ -> failwith "TODO"


                let source =
                    match source with
                    | Any.Network n -> n
                    | Any.Variable v ->
                        if variables.ContainsKey v then
                            match variables[v] with
                            | Any.Network n -> n
                            | _ -> failwith "TODO"
                        else
                            failwith "TODO"
                    | Any.Quote quote ->
                        match evalQuote commands variables quote with
                        | Ok((Some(Any.Network n), commands, variables)) -> n
                        | _ -> failwith "TODO"
                    | _ -> failwith "TODO"

                let results = filter pattern source
                Ok((Some(Any.Network results), commands, variables))
            | _ -> error "Invalid call to filter" None }

let networkCommands: Map<Element, Command> =
    (Map.ofList
        [ (applyCommand.Name, applyCommand)
          (chompCommand.Name, chompCommand)
          (countCommand.Name, countCommand)
          (minusCommand.Name, minusCommand)
          (matchCommand.Name, matchCommand)
          (queryCommand.Name, queryCommand)
          (unionCommand.Name, unionCommand)
          (filterCommand.Name, filterCommand)
          //(isCompleteCommand.Name, isCompleteCommand)
          //(isConsistentCommand.Name, isConsistentCommand)
          ])
