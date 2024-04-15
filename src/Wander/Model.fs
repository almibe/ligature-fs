// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Model

open Ligature
open System.Web
open System

[<RequireQualifiedAccess>]
type Expression =
    | Nothing
    | QuestionMark
    | Int of int64
    | Bytes of byte array
    | String of string
    | Bool of bool
    | Identifier of Identifier
    | Statement of Ligature.Statement
    | Let of name: string * value: Expression
    | NamePath of string list
    | Grouping of Expression list
    | Array of Expression list
    | Application of Expression list
    | FunctionCall of name: string * arguments: Expression list
    | Record of list<string * Expression>
    | Dataset of Expression list
    | When of list<Expression * Expression>
    | Lambda of list<string> * Expression

[<RequireQualifiedAccess>]
type WanderValue<'t> =
    | QuestionMark
    | Int of int64
    | String of string
    | Bool of bool
    | Identifier of Identifier
    | Statement of Ligature.Statement
    | Nothing
    | Function of Function
    | Array of WanderValue<'t> array
    | Dataset of IDataset
    | Record of Map<string, WanderValue<'t>>
    | Bytes of byte array
    | HostValue of 't

and [<RequireQualifiedAccess>] Function =
    | Lambda of paramters: string list * body: Expression
    | HostFunction of HostFunction

and HostFunction
    (eval: WanderValue<'t> list -> Bindings.Bindings<string, WanderValue<'t>> -> Result<WanderValue<'t>, LigatureError>)
    =
    member _.Run args bindings = eval args bindings

type Parameter = { name: string; tag: string }

type Bindings<'t> = Bindings.Bindings<string, WanderValue<'t>>

let rec prettyPrint (value: WanderValue<'t>) : string =
    match value with
    | WanderValue.Int i -> sprintf "%i" i
    | WanderValue.String s -> HttpUtility.JavaScriptStringEncode(s, true)
    | WanderValue.Bool b -> sprintf "%b" b
    | WanderValue.Identifier i -> $"`{(readIdentifier i)}`"
    | WanderValue.Nothing -> "Nothing"
    | WanderValue.Array(values) -> $"[{printValues values}]"
    | WanderValue.HostValue(_) -> "HostValue"
    | WanderValue.Statement(statement) -> printStatement statement
    | WanderValue.Record(values) -> printRecord values
    | WanderValue.QuestionMark -> "?"
    | WanderValue.Function(_) -> "Function"
    | WanderValue.Bytes(bytes) -> printBytes bytes
    | WanderValue.Dataset(_) -> "Dataset"

and printBytes bytes =
    bytes
    |> Array.map (fun value -> System.String.Format("{0:X2}", value))
    |> Array.insertAt 0 "0x"
    |> String.concat String.Empty

and printRecord values =
    "{ "
    + Map.fold (fun state key value -> state + $"{key} = {prettyPrint value}, ") "" values
    + "}"

and printStatement statement =
    $"`{(readIdentifier statement.Entity)}` `{(readIdentifier statement.Attribute)}` {(printLigatureValue statement.Value)}"

and printLigatureValue value =
    match value with
    | Value.Identifier(value) -> $"`{(readIdentifier value)}`"
    | Value.Integer(value) -> value.ToString()
    | Value.String(value) -> $"\"{value}\"" //TODO escape properly
    | Value.Bytes(bytes) -> printBytes bytes

and printValues values =
    Seq.fold (fun x y -> x + (prettyPrint y) + ", ") "" values
