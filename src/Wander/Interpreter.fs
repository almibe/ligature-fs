// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Interpreter

open Ligature.Model
open Model

let rec evalElement
    (network: Network)
    (local: Module)
    (modules: Modules)
    (arguments: Any list)
    (Element(name))
    : Result<CommandResult, LigatureError> =
    if name.Contains '.' then
        let moduleName = (name.Split '.')[0]
        let commandName = (name.Split '.')[1]

        match modules.TryFind(Element moduleName) with
        | Some(mdl) ->
            match mdl.TryFind(Element(commandName)) with
            | Some(command) -> command.Eval network local modules arguments
            | None -> error $"Could not find name {name} in module {moduleName}" None
        | None -> error $"Could not find module {moduleName}" None
    else
        match local.TryFind(Element(name)) with
        | Some(command) -> command.Eval network local modules arguments
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

and addClosure (closureDefinition: CommandDefinition) (commands: Module) : Module = failwith "TODO"
    // Map.add
    //     closureDefinition.name
    //     { Eval =
    //         fun networks local modules arguments ->
    //             if arguments.Length = closureDefinition.args.Length then
    //                 let newVariables =
    //                     List.fold
    //                         (fun state (name, value) -> Map.add name value state)
    //                         variables
    //                         (List.allPairs closureDefinition.args arguments)

    //                 evalQuote networks local modules newVariables closureDefinition.body
    //             else
                    // failwith "TODO" }
    //     commands

and evalScript
    (network: Network)
    (local: Module)
    (modules: Modules)
    (script: Script)
    : Result<CommandResult, LigatureError> =
    match script with
    | [] -> Ok(network, local, modules)
    | [ Expression.Call head ] -> evalCall network local modules head
    // | Expression.Call head :: tail ->
    //     match evalCall network local modules head with
    //     | Ok(_, networks, local, modules) -> evalScript networks local modules tail
    //     | Error err -> Error err
    | Expression.Network head :: tail ->
        Ok((Set.union head network), local, modules)
    // | Expression.AnyAssignment(variable, value) :: tail ->
    //     evalScript network local modules (Map.add variable value) tail
    // | Expression.CallAssignment(variable, call) :: tail ->
    //     match evalCall network local modules variables call with
    //     | Ok((Some value, networks, local, modules, variables)) ->
    //         evalScript networks local modules (Map.add variable value variables) tail
    //     | Ok(None, _, _, _, _) -> error "Expected value in assignment." None
    //     | Error err -> error $"Error in eval. {err.UserMessage}" None
    // | Expression.CommandDefinition closureDefinition :: tail ->
    //     evalScript network (addClosure closureDefinition local) modules variables tail

and evalCall
    (network: Network)
    (local: Module)
    (modules: Modules)
    ((name, args): Call)
    : Result<CommandResult, LigatureError> =
    evalElement network local modules args name

and evalQuote
    (network: Network)
    (local: Module)
    (modules: Modules)
    (quote: Quote)
    : Result<CommandResult, LigatureError> =
    match rewriteQuote quote with
    | Ok quote ->
        match quote with
        | [] -> failwith "TODO"
        | [ Any.Element name ] -> evalElement network local modules [] name
        | _ ->
            match quote.Head with
            | Any.Element name -> evalElement network local modules quote.Tail name
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
