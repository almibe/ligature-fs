// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Fns.Bend

open Ligature.Model
open Wander.Model
open Ligature.Core
open Wander.Interpreter

let bendJsonFn =
    Fn(
        { doc = ""
          examples = [ ]
          args = "Network Term Network"
          result = "Literal" },
        fun actions variables arguments ->
            match arguments with
            | [ Any.Network left; Any.Network right ] ->
                // let left =
                //     match left with
                //     | Any.Network n -> n
                //     | Any.Slot v ->
                //         match Map.tryFind v variables with
                //         | Some(Any.Network res) -> res
                //         | _ -> failwith "TODO"
                //     | _ -> failwith "TODO"

                // let right =
                //     match right with
                //     | Any.Network n -> n
                //     | Any.Slot v ->
                //         match Map.tryFind v variables with
                //         | Some(Any.Network res) -> res
                //         | _ -> failwith "TODO"
                //     | Any.Quote quote ->
                //         match evalQuote networks local modules variables quote with
                //         | Ok((Some(Any.Network network), _, _, _, _)) -> network
                //         | _ -> failwith "TODO"
                //     | _ -> failwith "TODO"
                let result = Set.union left right |> Any.Network
                Ok result
            | _ -> failwith $"Calls to union requires two Networks on the stack."
    )
