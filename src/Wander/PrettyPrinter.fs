// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.PrettyPrinter

[<RequireQualifiedAccess>]
type PrintToken =
    | Text of string
    | Break
    | Nest of int * PrintToken list
    | Group of PrintToken list

let rec format (token: PrintToken) : string =
    match token with
    | PrintToken.Text text -> text
    | PrintToken.Break -> ""
    | PrintToken.Group group ->
        List.fold
            (fun state value ->
                if state = "" then
                    format value
                else
                    state + " " + format value)
            ""
            group
    | PrintToken.Nest(_, _) -> failwith "TODO"
