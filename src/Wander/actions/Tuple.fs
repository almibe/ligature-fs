// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Fns.Tuple

open Wander.Model
open Ligature.Model


let resultSetFn =
    Fn(
        { doc = "Create a ResultSet."
          examples = [ "(result-set [{?a a}])" ]
          args = "Tuple"
          result = "ResultSet" },
        fun _ _ _ arguments -> failwith "TODO"
    // match arguments with
    // | [ Any.Tuple tuple ] ->
    //     let set =
    //         Set.ofList tuple
    //         |> Set.map (fun value ->
    //             match value with
    //             | Any.Node record ->
    //                 let res =
    //                     Seq.fold
    //                         (fun state value ->
    //                             match value with
    //                             | Any.Slot key, Any.Term value -> Map.add key (Value.Term value) state
    //                             | Any.Slot key, Any.Literal value -> Map.add key (Value.Literal value) state
    //                             | _ -> failwith "TODO")
    //                         Map.empty
    //                         (Map.toSeq record)

    //                 res
    //             | _ -> failwith "TODO")

    //     Ok(Any.ResultSet set)
    // | _ -> error "Invalid call to set action." None
    )
