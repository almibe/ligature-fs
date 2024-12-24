// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Commands.Network

open Ligature.Model
open Wander.Model
open Ligature.Core
open Wander.Interpreter

let isCompleteCommand =
    { Name = Element("is-complete")
      Doc = "Determine if a Network is complete."
      Eval = fun _ _ _ _ -> failwith "TODO" }
//             match arguments with
//             // | [ Value.Element(Element(networkName)) ] ->
//             //     let value = store.IsComplete networkName
//             //     Ok(Some(Value.Element(value.ToString().ToLower() |> Element)))
//             // | [ Value.Network(network) ] ->
//             //     let value = isComplete network
//             //     Ok(Some(Value.Element(value.ToString().ToLower() |> Element)))
//             | _ -> error "Bad call to is-complete." None }

let unionCommand =
    { Name = Element("union")
      Doc = "Find the union of two Networks."
      Eval =
        fun local modules variables (arguments: Arguments) ->
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
                        match evalQuote local modules variables quote with
                        | Ok((Some(Any.Network network), _, _, _)) -> network
                        | _ -> failwith "TODO"
                    | _ -> failwith "TODO"

                let result = Set.union left right |> Any.Network
                Ok((Some(result), local, modules, variables))
            | args -> failwith $"TODO - {args}" }

let countCommand =
    { Name = Element("count")
      Doc = "Count the number of assertions in a Network."
      Eval =
        fun local modules variables (arguments: Arguments) ->
            match arguments with
            | [ Any.Variable variable ] ->
                match variables.TryFind variable with
                | Some(Any.Network network) ->
                    Ok((Some(Any.Element(Element((Set.count network).ToString()))), local, modules, variables))
                | _ -> failwith "TODO"
            | [ Any.Network network ] ->
                Ok((Some(Any.Element(Element(network.Count.ToString()))), local, modules, variables))
            | [ Any.Quote quote ] ->
                match evalQuote local modules variables quote with
                | Ok((Some(Any.Network network), local, modules, variables)) ->
                    Ok((Some(Any.Element(Element(network.Count.ToString()))), local, modules, variables))
                | Ok(None, _, _, _) -> error "Error in count, expected value." None
                | Error err -> error $"Error in count, {err.UserMessage}" None
            | args -> failwith $"TODO - {args}" }

let minusCommand =
    { Name = Element("minus")
      Doc = "Remove all Statements from the first Network that are in the second Networks."
      Eval =
        fun local modules variables (arguments: Arguments) ->
            match arguments with
            | [ Any.Network(left); Any.Network(right) ] ->
                let result = Set.difference left right |> Any.Network
                Ok((Some(result), local, modules, variables))
            | _ -> failwith "TODO" }

let queryCommand =
    { Name = Element("query")
      Doc = "arguments: pattern template data, returns network"
      Eval =
        fun local modules variables arguments ->
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
                        match evalQuote local modules variables quote with
                        | Ok((Some(Any.Network n), local, modules, variables)) -> n
                        | _ -> failwith "TODO"
                    | _ -> failwith "TODO"

                let results = query pattern template source
                Ok((Some(Any.Network results), local, modules, variables))
            | _ -> error "Invalid call to query" None }

let matchCommand =
    { Name = Element "match"
      Doc = "Match a pattern against a network."
      Eval =
        fun local modules variables arguments ->
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

                Ok((Some(Any.ResultSet(singleMatch (element, attribute, value) network)), local, modules, variables))
            | [ pattern; network ] ->
                let pattern =
                    match pattern with
                    | Any.Network n -> n
                    | Any.Quote q ->
                        match evalQuote local modules variables q with
                        | Ok((Some(Any.Network n), local, modules, variables)) -> n
                        | _ -> failwith "TODO"
                    | _ -> failwith "TODO"

                let network =
                    match network with
                    | Any.Network n -> n
                    | Any.Quote q ->
                        match evalQuote local modules variables q with
                        | Ok((Some(Any.Network n), local, modules, variables)) -> n
                        | _ -> failwith "TODO"
                    | _ -> failwith "TODO"

                Ok(Some(Any.ResultSet(networkMatch pattern network)), local, modules, variables)

            | _ -> failwith "TODO" }

let applyCommand =
    { Name = Element "apply"
      Doc = "Fill in Variables in a Network with values from a Result Set."
      Eval =
        fun local modules variables arguments ->
            match arguments with
            | [ Any.Network network; Any.Quote q ] ->
                match evalQuote local modules variables q with
                | Ok((Some(Any.ResultSet res), local, modules, variables)) ->
                    let res = apply network res
                    Ok((Some(Any.Network res), local, modules, variables))
                | Ok((Some(Any.ValueSet res), local, modules, variables)) ->
                    let res = applyValueSet network res
                    Ok((Some(Any.Network res), local, modules, variables))
                | Ok _ -> failwith "TODO"
                | Error err -> error $"Error in apply. {err.UserMessage}" None
            | [ Any.Network network; Any.Variable v ] ->
                match Map.tryFind v variables with
                | Some(Any.ResultSet res) ->
                    let res = apply network res
                    Ok((Some(Any.Network res), local, modules, variables))
                | Some(Any.ValueSet res) ->
                    let res = applyValueSet network res
                    Ok((Some(Any.Network res), local, modules, variables))
                | Some _ -> failwith "TODO"
                | None -> failwith "TODO"
            | args -> failwith $"TODO - unexpected args {args}" }

let filterCommand =
    { Name = Element("filter")
      Doc = "arguments: pattern data, returns network"
      Eval =
        fun local modules variables arguments ->
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
                        match evalQuote local modules variables quote with
                        | Ok((Some(Any.Network n), local, modules, variables)) -> n
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
                        match evalQuote local modules variables quote with
                        | Ok((Some(Any.Network n), local, modules, variables)) -> n
                        | _ -> failwith "TODO"
                    | _ -> failwith "TODO"

                let results = filter pattern source
                Ok((Some(Any.Network results), local, modules, variables))
            | _ -> error "Invalid call to filter" None }

let networkCommands: Map<Element, Command> =
    (Map.ofList
        [ (applyCommand.Name, applyCommand)
          (countCommand.Name, countCommand)
          (minusCommand.Name, minusCommand)
          (matchCommand.Name, matchCommand)
          (queryCommand.Name, queryCommand)
          (unionCommand.Name, unionCommand)
          (filterCommand.Name, filterCommand)
          //(isCompleteCommand.Name, isCompleteCommand)
          //(isConsistentCommand.Name, isConsistentCommand)
          ])
