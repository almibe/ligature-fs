// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Interpreter

open Ligature.Main
open Model

//let evalNetworkName (networks: LigatureStore) (name: NetworkName) : Result<Identifier, LigatureError> = Ok(networks)

let evalNetwork
    (store: LigatureEngine)
    (name: NetworkName)
    (network: Set<Entry>)
    : Result<WanderValue option, LigatureError> =
    store.AddEntries name network |> ignore
    Ok None

let rec evalElement
    (commands: Commands)
    networks
    (arguments: WanderValue list)
    (Element(name))
    : Result<WanderValue option, LigatureError> =
    match commands.TryFind(Element(name)) with
    | Some(command) -> command.Eval commands networks arguments
    | None -> error $"Could not find name {name}" None

and processArguments commands networks (arguments: WanderValue list) : WanderValue list =
    List.map
        (fun argument ->
            match argument with
            | WanderValue.Call(n, e) ->
                match evalCall commands networks (n, e) with
                | Ok(Some(value)) -> value
                | _ -> WanderValue.Network Set.empty
            | value -> value)
        arguments

and evalCalls (commands: Commands) store (calls: Call list) : Result<WanderValue option, LigatureError> =
    match calls with
    | [] -> Ok(None)
    | [ head ] -> evalCall commands store head
    | head :: tail ->
        match evalCall commands store head with
        | Ok(value) -> evalCalls commands store tail
        | Error(err) -> Error(err)

and evalCall
    (commands: Commands)
    (store: LigatureEngine)
    ((name, args): Call)
    : Result<WanderValue option, LigatureError> =
    evalElement commands store args name
