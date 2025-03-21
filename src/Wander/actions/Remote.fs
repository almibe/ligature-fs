// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Fns.Remote

open Wander.Model
open Wander.Interpreter
open Ligature.Model
open FsHttp

let toScript (code: Tuple) : string =
    List.fold (fun state value -> state + printAny value + "\n") "" code

let remoteFn =
    Fn(
        { doc =
            "Reads a Literal for the address and a tuple for the code to execute remotely in a Tuple.\nAdds all returned values onto the current Stack."
          examples = [ "[docs] \"localhost:5000\" remote" ]
          args = "Literal Tuple"
          result = "Any..." },
        fun actions variables arguments -> failwith "TODO"
    // match stack with
    // | Any.Literal address :: Any.Tuple code :: tail ->
    //     let script = toScript code
    //     printfn $"Script = {script}"
    //     let res =
    //         http {
    //             POST address
    //             CacheControl "no-cache"
    //             body
    //             text script
    //         }
    //         |> Request.send
    //     let resText = res.ToText()
    //     printfn $"{resText}"
    //     match read resText with
    //     | Ok res -> failwith "TODO"
    //     | _ -> failwith "TODO"
    //     // failwith "TODO"
    //     //let newTuple = List.append source dest
    //     //Ok(Any.Tuple newTuple :: tail)
    // | _ -> error "Invalid call to prepend action." None
    )
