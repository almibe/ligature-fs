// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Main

open Parser
open Tokenizer
open Ligature.Main
open Interpreter
open Wander.Model

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

// type Introspect =
//     { tokens: Result<Token list, string>
//       elements: Result<WanderElement list, string>
//       expressions: Result<WanderElement list, string> }

// let introspect (input: string) =
//     match tokenize input with
//     | Ok tokens ->
//         match parse tokens with
//         | Ok elements ->
//             let expressions = failwith "TODO" //express elements []

//             { tokens = Ok tokens
//               elements = failwith "TODO" //Ok elements
//               expressions = Ok expressions }
//         | Error err ->
//             { tokens = Ok tokens
//               elements = Error(string err)
//               expressions = Error(string err) }
//     | Error err ->
//         { tokens = Error(string err)
//           elements = Error(string err)
//           expressions = Error(string err) }
