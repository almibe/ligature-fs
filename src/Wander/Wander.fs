// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Main

open Parser
open Tokenizer
open Ligature.Model
open Interpreter
open Wander.Model

let run (commands: Commands) (variables: Variables) (input: string) : Result<Any option, LigatureError> =
    try
        match tokenize input with
        | Ok tokens ->
            match parse tokens with
            | Ok script -> evalScript commands variables script
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
