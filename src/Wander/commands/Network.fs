// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Commands.Network

open Ligature.Model
open Wander.Model
open Ligature.Core
open Wander.Interpreter

let unionCommand =
    { Eval =
        fun networks local modules variables (arguments: Arguments) ->
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
                        match evalQuote networks local modules variables quote with
                        | Ok((Some(Any.Network network), _, _, _, _)) -> network
                        | _ -> failwith "TODO"
                    | _ -> failwith "TODO"

                let result = Set.union left right |> Any.Network
                Ok((Some(result), networks, local, modules, variables))
            | args -> failwith $"TODO - {args}" }

let setCommand =
    { Eval =
        fun networks local modules variables (arguments: Arguments) ->
            match arguments with
            | [ name; network ] ->
                let name =
                    match name with
                    | Any.Element networkName -> networkName
                    | _ -> failwith "TODO"

                let network =
                    match network with
                    | Any.Network n -> n
                    | Any.Quote quote ->
                        match evalQuote networks local modules variables quote with
                        | Ok((Some(Any.Network network), _, _, _, _)) -> network
                        | _ -> failwith "TODO"
                    | _ -> failwith "TODO"

                let networks = Map.add name network networks
                Ok((None, networks, local, modules, variables))
            | args -> failwith $"TODO - {args}" }

let readCommand =
    { Eval =
        fun networks local modules variables (arguments: Arguments) ->
            match arguments with
            | [ name ] ->
                let name =
                    match name with
                    | Any.Element networkName -> networkName
                    | _ -> failwith "TODO"

                match Map.tryFind name networks with
                | Some(network) -> Ok((Some(Any.Network network), networks, local, modules, variables))
                | None -> Ok((None, networks, local, modules, variables))
            | args -> failwith $"TODO - {args}" }

let mergeCommand =
    { Eval =
        fun networks local modules variables (arguments: Arguments) ->
            match arguments with
            | [ name; network ] ->
                let networkName =
                    match name with
                    | Any.Element networkName -> networkName
                    | _ -> failwith "TODO"

                let originalNetwork =
                    match Map.tryFind networkName networks with
                    | Some network -> network
                    | None -> failwith "TODO"

                let network =
                    match network with
                    | Any.Network n -> n
                    | Any.Quote quote ->
                        match evalQuote networks local modules variables quote with
                        | Ok((Some(Any.Network network), _, _, _, _)) -> network
                        | _ -> failwith "TODO"
                    | _ -> failwith "TODO"
                let result = Set.union originalNetwork network
                let networks = Map.add networkName result networks
                Ok((Some(Any.Network result), networks, local, modules, variables))
            | args -> failwith $"TODO - {args}" }

let countCommand =
    { Eval =
        fun networks local modules variables (arguments: Arguments) ->
            match arguments with
            | [ Any.Variable variable ] ->
                match variables.TryFind variable with
                | Some(Any.Network network) ->
                    Ok(
                        (Some(Any.Element(Element((Set.count network).ToString()))), networks, local, modules, variables)
                    )
                | _ -> failwith "TODO"
            | [ Any.Network network ] ->
                Ok((Some(Any.Element(Element(network.Count.ToString()))), networks, local, modules, variables))
            | [ Any.Quote quote ] ->
                match evalQuote networks local modules variables quote with
                | Ok((Some(Any.Network network), networks, local, modules, variables)) ->
                    Ok((Some(Any.Element(Element(network.Count.ToString()))), networks, local, modules, variables))
                | Ok(None, _, _, _, _) -> error "Error in count, expected value." None
                | Error err -> error $"Error in count, {err.UserMessage}" None
            | args -> failwith $"TODO - {args}" }

let minusCommand =
    { Eval =
        fun networks local modules variables (arguments: Arguments) ->
            match arguments with
            | [ Any.Network(left); Any.Network(right) ] ->
                let result = Set.difference left right |> Any.Network
                Ok((Some(result), networks, local, modules, variables))
            | _ -> failwith "TODO" }

let queryCommand =
    { Eval =
        fun networks local modules variables arguments ->
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
                        match evalQuote networks local modules variables quote with
                        | Ok((Some(Any.Network n), networks, local, modules, variables)) -> n
                        | _ -> failwith "TODO"
                    | _ -> failwith "TODO"

                let results = query pattern template source
                Ok((Some(Any.Network results), networks, local, modules, variables))
            | _ -> error "Invalid call to query" None }

let matchCommand =
    { Eval =
        fun networks local modules variables arguments ->
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

                Ok(
                    (Some(Any.ResultSet(singleMatch (element, attribute, value) network)),
                     networks,
                     local,
                     modules,
                     variables)
                )
            | [ pattern; network ] ->
                let pattern =
                    match pattern with
                    | Any.Network n -> n
                    | Any.Quote q ->
                        match evalQuote networks local modules variables q with
                        | Ok((Some(Any.Network n), networks, local, modules, variables)) -> n
                        | _ -> failwith "TODO"
                    | _ -> failwith "TODO"

                let network =
                    match network with
                    | Any.Network n -> n
                    | Any.Quote q ->
                        match evalQuote networks local modules variables q with
                        | Ok((Some(Any.Network n), networks, local, modules, variables)) -> n
                        | _ -> failwith "TODO"
                    | _ -> failwith "TODO"

                Ok(Some(Any.ResultSet(networkMatch pattern network)), networks, local, modules, variables)

            | _ -> failwith "TODO" }

let applyCommand =
    { Eval =
        fun networks local modules variables arguments ->
            match arguments with
            | [ Any.Network network; Any.Quote q ] ->
                match evalQuote networks local modules variables q with
                | Ok((Some(Any.ResultSet res), networks, local, modules, variables)) ->
                    let res = apply network res
                    Ok((Some(Any.Network res), networks, local, modules, variables))
                | Ok((Some(Any.ValueSet res), networks, local, modules, variables)) ->
                    let res = applyValueSet network res
                    Ok((Some(Any.Network res), networks, local, modules, variables))
                | Ok _ -> failwith "TODO"
                | Error err -> error $"Error in apply. {err.UserMessage}" None
            | [ Any.Network network; Any.Variable v ] ->
                match Map.tryFind v variables with
                | Some(Any.ResultSet res) ->
                    let res = apply network res
                    Ok((Some(Any.Network res), networks, local, modules, variables))
                | Some(Any.ValueSet res) ->
                    let res = applyValueSet network res
                    Ok((Some(Any.Network res), networks, local, modules, variables))
                | Some _ -> failwith "TODO"
                | None -> failwith "TODO"
            | args -> failwith $"TODO - unexpected args {args}" }

let filterCommand =
    { Eval =
        fun networks local modules variables arguments ->
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
                        match evalQuote networks local modules variables quote with
                        | Ok((Some(Any.Network n), networks, local, modules, variables)) -> n
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
                        match evalQuote networks local modules variables quote with
                        | Ok((Some(Any.Network n), networks, local, modules, variables)) -> n
                        | _ -> failwith "TODO"
                    | _ -> failwith "TODO"

                let results = filter pattern source
                Ok((Some(Any.Network results), networks, local, modules, variables))
            | _ -> error "Invalid call to filter" None }

let networkCommands: Map<Element, Command> =
    (Map.ofList
        [ (Element "apply", applyCommand)
          (Element "count", countCommand)
          (Element "minus", minusCommand)
          (Element "match", matchCommand)
          (Element "query", queryCommand)
          (Element "union", unionCommand)
          (Element "merge", mergeCommand)
          (Element "set", setCommand)
          (Element "read", readCommand)
          (Element "filter", filterCommand)
          //(isCompleteCommand.Name, isCompleteCommand)
          //(isConsistentCommand.Name, isConsistentCommand)
          ])
