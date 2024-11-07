// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Main

open Parser
open Tokenizer
open Ligature.Main
open Interpreter
open Wander.Model
open Ligature.InMemoryStore

let run (commands: Commands) (store: LigatureStore) (input: string) : Result<WanderValue option, LigatureError> =
    try
        match tokenize input with
        | Ok tokens ->
            match parse tokens with
            | Ok calls -> evalCalls commands store calls
            | Error(err) -> error $"Error parsing.\n{err}" None
        | Error _ -> error "Error tokenizing." None
    with x ->
        error $"Error running script. {x}" None

type WanderEngine =
    abstract member Run: script: string -> Result<WanderValue option, LigatureError>
    abstract member AddCommand: Symbol -> Command -> unit
    abstract member Store: unit -> LigatureStore

type DefaultEngine (commands: Commands, store: LigatureStore) =
    let mutable commands: Commands = commands
    let mutable store: LigatureStore = store

    interface WanderEngine with
        member _.Run(script) = 
            run commands store script
        member _.AddCommand(name: Symbol) (command: Command): unit = 
            commands <- Map.add name command commands
        member _.Store(): LigatureStore =
            store

let defaultEngine = new DefaultEngine(Map.empty, emptyInMemoryStore())
