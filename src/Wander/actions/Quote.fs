// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Actions.Quote

open Wander.Model
open Ligature.Model

let prependAction =
    Action.Stack({ doc = "Read a Term and a Quote off the Stack, push a new Quote with the Term at the front." }, fun stack ->
        match stack with
        | Any.Quote source :: Any.Quote dest :: tail -> 
            let newQuote = List.append source dest
            Ok(Any.Quote newQuote :: tail)
        | _ -> error "Invalid call to prepend action." None)
