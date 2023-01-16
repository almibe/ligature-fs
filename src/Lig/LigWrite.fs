// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Lig.Write

open Ligature

let inline todo<'T> : 'T = raise (System.NotImplementedException("todo"))

let writeIdentifier identifier =
    "<" + (readIdentifier identifier) + ">"

let writeString string =
    let chars = [| for c in string -> c |]
    let buffer = new System.Text.StringBuilder()
    buffer.Append("\"") |> ignore
    for c in chars do
        //TODO check for special chars and escape
        buffer.Append(c.ToString()) |> ignore
    buffer.Append('"') |> ignore
    buffer.ToString()

let writeBytes bytes = todo

let writeValue (value: Value) =
    match value with
    | Identifier(identifier) -> writeIdentifier identifier
    | String(string) -> writeString string
    | Integer(int64) -> int64.ToString()
    //| Bytes(bytes) -> writeBytes bytes

let writeStatement (statement: Statement) =
    writeIdentifier(statement.Entity) + " " + 
        writeIdentifier(statement.Attribute) + " " +
        writeValue(statement.Value)

let writeLig (statements: List<Statement>) =
    let sb = new System.Text.StringBuilder()
    for statement in statements do
        sb.Append((writeStatement statement) + "\n") |> ignore
    sb.ToString()
