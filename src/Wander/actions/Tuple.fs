// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Fns.Tuple

open Wander.Model
open Ligature.Model

let prependFn =
    Fn(
        { doc = "Read a Tuple and a Tuple off the Stack, push a new Tuple with the first Tuple at the front."
          examples = []
          args = "Tuple Tuple"
          result = "Tuple" },
        fun actions variables arguments -> failwith "TODO"
    // match stack with
    // | Any.Tuple source :: Any.Tuple dest :: tail ->
    //     let newTuple = List.append source dest
    //     Ok(Any.Tuple newTuple :: tail)
    // | _ -> error "Invalid call to prepend action." None
    )

let setFn =
    Fn(
        { doc = "Read a Tuple off the Stack and convert it to a Set and push the new Set on the Stack."
          examples = []
          args = "Tuple"
          result = "Set" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Tuple tuple ] ->
                let set = Set.ofList tuple
                Ok(Any.AnySet set)
            | _ -> error "Invalid call to set action." None
    )

let resultSetFn =
    Fn(
        { doc = "Read a Tuple off the Stack and convert it to a Set and push the new Set on the Stack."
          examples = [ "(result-set [{?a a}])" ]
          args = "Tuple"
          result = "ResultSet" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Tuple tuple ] ->
                let set =
                    Set.ofList tuple
                    |> Set.map (fun value ->
                        match value with
                        | Any.Record record ->
                            let res =
                                Seq.fold
                                    (fun state value ->
                                        match value with
                                        | Any.Slot key, Any.Term value -> Map.add key (Value.Term value) state
                                        | Any.Slot key, Any.Literal value -> Map.add key (Value.Literal value) state
                                        | _ -> failwith "TODO")
                                    Map.empty
                                    (Map.toSeq record)

                            res
                        | _ -> failwith "TODO")

                Ok(Any.ResultSet set)
            | _ -> error "Invalid call to set action." None
    )
