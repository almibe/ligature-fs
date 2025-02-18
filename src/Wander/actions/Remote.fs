// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Actions.Remote

open Wander.Model
open Wander.Interpreter
open Ligature.Model
open FsHttp

let toScript (code: Quote) : string =
    List.fold (fun state value -> state + printAny value + "\n") "" code

let remoteAction =
    Action.Stack(
        { doc =
            "Reads a Literal for the address and a quote for the code to execute remotely in a Quote.\nAdds all returned values onto the current Stack."
          examples = [ "[docs] \"localhost:5000\" remote" ]
          pre = "Literal Quote"
          post = "Any..." },
        fun stack -> failwith "TODO"
    // match stack with
    // | Any.Literal address :: Any.Quote code :: tail ->
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
    //     //let newQuote = List.append source dest
    //     //Ok(Any.Quote newQuote :: tail)
    // | _ -> error "Invalid call to prepend action." None
    )
