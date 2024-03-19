// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Lig.Write

open Ligature

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

let writeIdentifier identifier = "<" + (readLabel identifier) + ">"

let writeString string =
    let chars = [| for c in string -> c |]
    let buffer = System.Text.StringBuilder()
    buffer.Append("\"") |> ignore

    for c in chars do
        //TODO check for special chars and escape
        buffer.Append(c.ToString()) |> ignore

    buffer.Append('"') |> ignore
    buffer.ToString()

let writeBytes bytes = todo

let writeValue (value: Value) =
    match value with
    | Label(identifier) -> writeIdentifier identifier
    | String(string) -> writeString string
    | Integer(int64) -> int64.ToString()
//| Bytes(bytes) -> writeBytes bytes

let writeStatement (statement: Edge) =
    writeIdentifier (statement.Source)
    + " "
    + writeIdentifier (statement.Label)
    + " "
    + writeValue (statement.Target)

let writeLig (statements: List<Edge>) =
    let sb = System.Text.StringBuilder()

    for statement in statements do
        sb.Append((writeStatement statement) + "\n") |> ignore

    sb.ToString()
