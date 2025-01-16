// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Interpreter

open Ligature.Model
open Model

// let rec evalElement
//     (actions: Actions)
//     (network: Network)
//     (Element(name))
//     : Result<Network, LigatureError> =
//     if name.Contains '.' then
//         let moduleName = (name.Split '.')[0]
//         let commandName = (name.Split '.')[1]
//         failwith "TODO"
//         // match modules.TryFind(Element moduleName) with
//         // | Some(mdl) ->
//         //     match mdl.TryFind(Element(commandName)) with
//         //     | Some(command) -> command.Eval network local modules arguments
//         //     | None -> error $"Could not find name {name} in module {moduleName}" None
//         // | None -> error $"Could not find module {moduleName}" None
//     else
//         failwith "TODO"
//         // match local.TryFind(Element(name)) with
//         // | Some(command) -> command.Eval network local modules arguments
//         // | None -> error $"Could not find name {name}" None

let resolveActions (actions: Actions) (network: Network) : Result<Network, LigatureError> =
    let actionElements =
        Seq.choose
            (fun triple ->
                match triple with
                | ElementPattern.Element(Element("action")),
                  ElementPattern.Element(Element("=")),
                  Value.Element(action) -> Some(action)
                | _ -> None)
            network

    Seq.fold
        (fun network action ->
            match network with
            | Ok network ->
                match Map.tryFind action actions with
                | Some action -> failwith "TOOD"
                | _ -> error $"Could not read action {action}." None
            | Error err -> Error err)
        (Ok network)
        actionElements

let rec evalScript (actions: Actions) (network: Network) (script: Script) : Result<Network, LigatureError> =
    match script with
    | [] -> Ok(network)
    | [ head ] -> resolveActions actions (Set.union head network)
    | head :: tail ->
        match resolveActions actions (Set.union head network) with
        | Ok(network) -> evalScript actions network tail
        | _ -> failwith "TODO"

// and evalQuote
//     (actions: Actions)
//     (network: Network)
//     (quote: Quote)
//     : Result<Network, LigatureError> =
//     match quote with
//     | [] -> failwith "TODO"
//     | [ Any.Element name ] -> evalElement actions network name
//     | _ ->
//         match quote.Head with
//         | Any.Element name -> evalElement actions network quote.Tail name
//         | _ -> failwith "TODO"
