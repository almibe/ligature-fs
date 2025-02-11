// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Actions.Remote

open Wander.Model
open Ligature.Model

let remoteAction =
    Action.Stack(
        { doc = "Reads a Literal for the address and a quote for the code to execute remotely in a Quote.\nAdds all returned values onto the current Stack."
          examples = ["[docs] \"localhost:5000/test\" remote"]
          pre = "Literal Quote"
          post = "Any..." },
        fun stack ->
            match stack with
            | Any.Literal address :: Any.Quote code :: tail ->
                task {
                    use client = new HttpClient()
                    let! response = client.GetStringA
                }
                let newQuote = List.append source dest
                Ok(Any.Quote newQuote :: tail)
            | _ -> error "Invalid call to prepend action." None
    )
