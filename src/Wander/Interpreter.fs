// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Interpreter

open Ligature.Model
open Model

let rec evalElement
    (networks: Networks)
    (local: Module)
    (modules: Modules)
    (variables: Variables)
    (arguments: Any list)
    (Element(name))
    : Result<CommandResult, LigatureError> =
    if name.Contains '.' then
        let moduleName = (name.Split '.')[0]
        let commandName = (name.Split '.')[1]

        match modules.TryFind(Element moduleName) with
        | Some(mdl) ->
            match mdl.TryFind(Element(commandName)) with
            | Some(command) -> command.Eval networks local modules variables arguments
            | None -> error $"Could not find name {name} in module {moduleName}" None
        | None -> error $"Could not find module {moduleName}" None
    else
        match local.TryFind(Element(name)) with
        | Some(command) -> command.Eval networks local modules variables arguments
        | None -> error $"Could not find name {name}" None

// and processArguments local commands networks (arguments: Any list) : Any list =
//     List.map
//         (fun argument ->
//             match argument with
//             | Any.Quote quote ->
//                 match evalQuote local commands networks quote with
//                 | Ok((Some(value), _, _, _)) -> value
//                 | _ -> Any.Network Set.empty
//             | value -> value)
//         arguments

and addClosure (closureDefinition: CommandDefinition) (commands: Module) : Module =
    Map.add
        closureDefinition.name
        { Eval =
            fun networks local modules variables arguments ->
                if arguments.Length = closureDefinition.args.Length then
                    let newVariables =
                        List.fold
                            (fun state (name, value) -> Map.add name value state)
                            variables
                            (List.allPairs closureDefinition.args arguments)

                    evalQuote networks local modules newVariables closureDefinition.body
                else
                    failwith "TODO" }
        commands

and evalScript
    (networks: Networks)
    (local: Module)
    (modules: Modules)
    (variables: Variables)
    (script: Script)
    : Result<CommandResult, LigatureError> =
    match script with
    | [] -> Ok(None, networks, local, modules, variables)
    | [ Expression.Call head ] -> evalCall networks local modules variables head
    | Expression.Call head :: tail ->
        match evalCall networks local modules variables head with
        | Ok(_, networks, local, modules, variables) -> evalScript networks local modules variables tail
        | Error err -> Error err
    | Expression.AnyAssignment(variable, value) :: tail ->
        evalScript networks local modules (Map.add variable value variables) tail
    | Expression.CallAssignment(variable, call) :: tail ->
        match evalCall networks local modules variables call with
        | Ok((Some value, networks, local, modules, variables)) ->
            evalScript networks local modules (Map.add variable value variables) tail
        | Ok(None, _, _, _, _) -> error "Expected value in assignment." None
        | Error err -> error $"Error in eval. {err.UserMessage}" None
    | Expression.CommandDefinition closureDefinition :: tail ->
        evalScript networks (addClosure closureDefinition local) modules variables tail

and evalCall
    (networks: Networks)
    (local: Module)
    (modules: Modules)
    (variables: Variables)
    ((name, args): Call)
    : Result<CommandResult, LigatureError> =
    evalElement networks local modules variables args name

and evalQuote
    (networks: Networks)
    (local: Module)
    (modules: Modules)
    (variables: Variables)
    (quote: Quote)
    : Result<CommandResult, LigatureError> =
    match rewriteQuote quote with
    | Ok quote ->
        match quote with
        | [] -> failwith "TODO"
        | [ Any.Element name ] -> evalElement networks local modules variables [] name
        | _ ->
            match quote.Head with
            | Any.Element name -> evalElement networks local modules variables quote.Tail name
            | _ -> failwith "TODO"
    | _ -> failwith "TODO"

and rewriteQuote (quote: Quote) : Result<Quote, LigatureError> =
    let mutable quotes: List<List<Any>> = List.empty
    let mutable current: List<Any> = List.empty

    //chunk
    List.iter
        (fun item ->
            match item with
            | Any.Pipe ->
                quotes <- List.append quotes [ current ]
                current <- List.empty
            | value -> current <- List.append current [ value ])
        quote

    quotes <- List.append quotes [ current ]

    //process
    match quotes with
    | [] -> Ok []
    | [ single ] -> Ok single
    | _ ->
        let res =
            List.reduce (fun state item -> List.append item [ Any.Quote state ]) quotes

        Ok res
