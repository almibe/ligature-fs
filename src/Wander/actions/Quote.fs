// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Actions.Quote

open Wander.Model
open Ligature.Model

let prependAction =
    Action.Stack(
        { doc = "Read a Quote and a Quote off the Stack, push a new Quote with the first Quote at the front."
          examples = []
          pre = "Quote Quote"
          post = "Quote" },
        fun stack -> failwith "TODO"
    // match stack with
    // | Any.Quote source :: Any.Quote dest :: tail ->
    //     let newQuote = List.append source dest
    //     Ok(Any.Quote newQuote :: tail)
    // | _ -> error "Invalid call to prepend action." None
    )

let setAction =
    Action.Stack(
        { doc = "Read a Quote off the Stack and convert it to a Set and push the new Set on the Stack."
          examples = []
          pre = "Quote"
          post = "Set" },
        fun stack -> failwith "TODO"
    // match stack with
    // | Any.Quote quote :: tail ->
    //     let set = Set.ofList quote
    //     Ok(Any.AnySet set :: tail)
    // | _ -> error "Invalid call to set action." None
    )
