// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Lib

open Ligature.Wander.Model
open Ligature.Main
open Ligature.Wander.Main
open Ligature.Wander.Interpreter

// let writeValueFunction =
//     { Name = "writeValue"
//       Returns = WanderType.String
//       Eval =
//         (fun args _ ->
//             match args with
//             | [ value ] -> Ok(WanderValue.String(prettyPrint value))
//             | value -> error $"Unexpected value - {value}." None) }

// let readValueFunction =
//     { Name = "readValue"
//       Returns = WanderType.Value
//       Eval =
//         (fun args _ ->
//             match args with
//             | [ WanderValue.String(input) ] -> run input (newBindings ())
//             | value -> error $"Unexpected value - {value}." None) }

let stdLib: Map<string, Word> =
    Map
        [ ("pop",
           { Eval =
               fun environment ->
                   match environment.Stack with
                   | [] -> Ok([]) //TODO maybe have this be an error?
                   | [ _ ] -> Ok([])
                   | _ :: tail -> Ok(tail) })
          ("dup",
           { Eval =
               fun environment ->
                   match environment.Stack with
                   | [] -> Ok([]) //TODO maybe have this be an error?
                   | [ head ] -> Ok([ head; head ])
                   | head :: tail -> Ok(head :: head :: tail) })
          ("apply",
           { Eval =
               fun environment ->
                   match environment.Stack |> List.tryHead with
                   | None -> Ok([]) //TODO maybe have this be an error?
                   | Some(WanderValue.Quote(head)) ->
                       match
                           evalValues
                               { environment with
                                   Stack = (environment.Stack.Tail) }
                               head
                       with
                       | Ok(res) -> Ok(res @ (List.tail environment.Stack))
                       | Error(err) -> failwith "TODO"
                   | Some(_) -> failwith "TODO" }) ]
