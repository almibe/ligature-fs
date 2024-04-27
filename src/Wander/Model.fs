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
    | Slot of Slot
    | Let of name: string * value: Expression
    | NamePath of string list
    | Grouping of Expression list
    | Array of Expression list
    | Application of Expression list
    | FunctionCall of name: string * arguments: Expression list
    | Record of list<string * Expression>
    | Pattern of DatasetPatternRoot list
    | Query of Expression * list<Expression * Expression>
    | Lambda of list<string> * Expression

and EntityDescription = Expression * Expression list

and DatasetPatternRoot = Expression * EntityDescription list

[<RequireQualifiedAccess>]
type WanderValue =
    | Int of bigint
    | String of string
    | Bool of bool
    | Identifier of Identifier
    | Slot of Slot
    | Statement of Ligature.Statement
    | Function of Function
    | Array of WanderValue array
    | Pattern of IPattern
    | Record of Map<string, WanderValue>
    | Bytes of byte array

and [<RequireQualifiedAccess>] Function =
    | Lambda of paramters: string list * body: Expression
    | HostFunction of HostFunction

and HostFunction(eval: WanderValue list -> Bindings.Bindings<string, WanderValue> -> Result<WanderValue, LigatureError>)
    =
    member _.Run args bindings = eval args bindings

type Parameter = { name: string; tag: string }

type Bindings = Bindings.Bindings<string, WanderValue>

let rec prettyPrint (value: WanderValue) : string =
    match value with
    | WanderValue.Int i -> sprintf "%A" i
    | WanderValue.String s -> HttpUtility.JavaScriptStringEncode(s, true)
    | WanderValue.Bool b -> sprintf "%b" b
    | WanderValue.Identifier i -> $"`{(readIdentifier i)}`"
    | WanderValue.Slot s -> $"${(s.Name)}"
    | WanderValue.Array(values) -> $"[{printValues values}]"
    | WanderValue.Statement(statement) -> printStatementLiteral statement
    | WanderValue.Record(values) -> printRecord values
    | WanderValue.Function(_) -> "Function"
    | WanderValue.Bytes(bytes) -> printBytes bytes
    | WanderValue.Pattern(values) ->
        (Set.fold (fun state statement -> state + " " + (printPattern statement) + ", ") "{" values.Statements)
        + "}"

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

and printPatternIdentifier (patternIdentifier: PatternIdentifier) =
    match patternIdentifier with
    | PatternIdentifier.Identifier(identifier) -> $"`{readIdentifier identifier}`"
    | PatternIdentifier.Slot(slot) -> $"${(slot.Name)}"

and printPatternValue (value: PatternValue) =
    match value with
    | PatternValue.Value value ->
        match value with
        | Value.Identifier(value) -> $"`{(readIdentifier value)}`"
        | Value.Int(value) -> value.ToString()
        | Value.String(value) -> $"\"{value}\"" //TODO escape properly
        | Value.Bytes(bytes) -> printBytes bytes
    | PatternValue.Slot(slot) -> $"${(slot.Name)}"

and printPattern (pattern: PatternStatement) =
    $"{(printPatternIdentifier pattern.Entity)} {(printPatternIdentifier pattern.Attribute)} {(printPatternValue pattern.Value)}"

and printLigatureValue value =
    match value with
    | Value.Identifier(value) -> $"`{(readIdentifier value)}`"
    | Value.Int(value) -> value.ToString()
    | Value.String(value) -> $"\"{value}\"" //TODO escape properly
    | Value.Bytes(bytes) -> printBytes bytes

and printValues values =
    Seq.fold (fun x y -> x + (prettyPrint y) + ", ") "" values
