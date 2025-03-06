// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Fns.Quote

open Wander.Model
open Ligature.Model

let prependFn =
    Fn(
        { doc = "Read a Quote and a Quote off the Stack, push a new Quote with the first Quote at the front."
          examples = []
          args = "Quote Quote"
          result = "Quote" },
        fun actions variables arguments -> failwith "TODO"
    // match stack with
    // | Any.Quote source :: Any.Quote dest :: tail ->
    //     let newQuote = List.append source dest
    //     Ok(Any.Quote newQuote :: tail)
    // | _ -> error "Invalid call to prepend action." None
    )

let setFn =
    Fn(
        { doc = "Read a Quote off the Stack and convert it to a Set and push the new Set on the Stack."
          examples = []
          args = "Quote"
          result = "Set" },
        fun actions variables arguments ->
            match arguments with
            | [ Any.Quote quote ] ->
                let set = Set.ofList quote
                Ok(Any.AnySet set)
            | _ -> error "Invalid call to set action." None
    )
