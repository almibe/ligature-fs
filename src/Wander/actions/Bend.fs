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
          examples = []
          args = "Network Term Network"
          result = "Literal" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Assertions definitions; Any.Term encodingName; Any.Assertions data ] ->
                let res = query (Set.ofList []) definitions

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
                //     | Any.Tuple tuple ->
                //         match evalTuple networks local modules variables tuple with
                //         | Ok((Some(Any.Network network), _, _, _, _)) -> network
                //         | _ -> failwith "TODO"
                //     | _ -> failwith "TODO"
                // let result = Set.union left right |> Any.Network
                // Ok result
                failwith "TODO"
            | _ -> failwith "Illegal call to bend.json."
    )
