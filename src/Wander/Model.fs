// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Model

open Ligature
open System.Web
open System

[<RequireQualifiedAccess>]
type Expression =
    | Colon
    | Int of bigint
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
    | Dataset of DatasetRoot list
    | Match of Expression * list<Expression * Expression>
    | Lambda of list<string> * Expression

and DatasetRoot = Expression * Expression * Expression list

[<RequireQualifiedAccess>]
type WanderValue<'t> =
    | Int of bigint
    | String of string
    | Bool of bool
    | Identifier of Identifier
    | Statement of Ligature.Statement
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
    | WanderValue.Int i -> sprintf "%A" i
    | WanderValue.String s -> HttpUtility.JavaScriptStringEncode(s, true)
    | WanderValue.Bool b -> sprintf "%b" b
    | WanderValue.Identifier i -> $"`{(readIdentifier i)}`"
    | WanderValue.Array(values) -> $"[{printValues values}]"
    | WanderValue.HostValue(_) -> "HostValue"
    | WanderValue.Statement(statement) -> printStatementLiteral statement
    | WanderValue.Record(values) -> printRecord values
    | WanderValue.Function(_) -> "Function"
    | WanderValue.Bytes(bytes) -> printBytes bytes
    | WanderValue.Dataset(values) ->
        match values.AllStatements() with
        | Ok statements ->
            (List.fold (fun state statement -> state + " " + (printStatement statement) + ", ") "{" statements)
            + "}"
        | Error err -> failwith ""

and printBytes bytes =
    bytes
    |> Array.map (fun value -> System.String.Format("{0:X2}", value))
    |> Array.insertAt 0 "0x"
    |> String.concat String.Empty

and printRecord values =
    "{ "
    + Map.fold (fun state key value -> state + $"{key} = {prettyPrint value}, ") "" values
    + "}"

and printStatementLiteral statement =
    $"(`{(readIdentifier statement.Entity)}` `{(readIdentifier statement.Attribute)}` {(printLigatureValue statement.Value)})"

and printStatement statement =
    $"`{(readIdentifier statement.Entity)}` `{(readIdentifier statement.Attribute)}` {(printLigatureValue statement.Value)}"

and printLigatureValue value =
    match value with
    | Value.Identifier(value) -> $"`{(readIdentifier value)}`"
    | Value.Int(value) -> value.ToString()
    | Value.String(value) -> $"\"{value}\"" //TODO escape properly
    | Value.Bytes(bytes) -> printBytes bytes

and printValues values =
    Seq.fold (fun x y -> x + (prettyPrint y) + ", ") "" values
