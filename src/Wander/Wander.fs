// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Main

open Parser
open Tokenizer
open Ligature.Model
open Interpreter
open Wander.Model

let run (actions: Actions) (networks: Networks) (stack: Stack) (input: string) : Result<Networks * Stack, LigatureError> =
    try
        match tokenize input with
        | Ok tokens ->
            match parse tokens with
            | Ok script ->
                match evalScript actions networks stack script with
                | Ok(network, stack) -> Ok(networks, stack)
                | Error err -> Error err
            | Error(err) -> error $"Error parsing.\n{err}" None
        | Error _ -> error "Error tokenizing." None
    with x ->
        error $"Error running script. {x}" None

let read (input: string) : Result<Any, LigatureError> =
    try
        match tokenize input with
        | Ok tokens -> read tokens
        | Error _ -> error "Error tokenizing." None
    with x ->
        error $"Error reading {x}" None
