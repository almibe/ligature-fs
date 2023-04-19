// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Model

open Ligature

[<RequireQualifiedAccess>]
type WanderType =
    | Any
    | Unspecified
    | Integer
    | String
    | Boolean
    | Identifier
    | Seq
    | Function
    | Graph
    | Nothing

type NativeFunction<'T, 'Output>(eval: 'T list -> Bindings.Bindings<string, 'Output> -> Result<'Output, LigatureError>) =
    member _.Run args bindings = eval args bindings

type Tuple<'T> = 'T list

[<RequireQualifiedAccess>]
type WanderValue<'T> =
    | Integer of int64
    | String of string
    | Boolean of bool
    | Identifier of Identifier
    | Nothing
    | Lambda of paramters: string list * body: 'T list
    | NativeFunction of NativeFunction<'T, WanderValue<'T>>
    | Tuple of Tuple<WanderValue<'T>>

type Parameter =
    { name: string; wanderType: WanderType }

type Case<'Condition, 'Body> = { condition: 'Condition; body: 'Body }

type Conditional<'Condition, 'Body> =
    { ifCase: Case<'Condition, 'Body>
      elsifCases: Case<'Condition, 'Body> list
      elseBody: 'Body }

[<RequireQualifiedAccess>]
type Expression =
    | LetStatement of name: string * value: Expression
    | Name of string
    | Scope of Expression list
    | Value of WanderValue<Expression>
    | FunctionCall of name: string * arguments: Expression list
    | Conditional of Conditional<Expression, Expression>
    | TupleExpression of Expression list

type WanderValue = WanderValue<Expression>
type Case = Case<Expression, Expression>
type Conditional = Conditional<Expression, Expression>
type NativeFunction = NativeFunction<Expression, WanderValue>
type Bindings = Bindings.Bindings<string, WanderValue>

let rec prettyPrint (value: WanderValue): string =
    match value with
    | WanderValue.Integer i -> sprintf "%i" i
    | WanderValue.String s -> s
    | WanderValue.Boolean b -> sprintf "%b" b
    | WanderValue.Identifier i -> $"<{(readIdentifier i)}>"
    | WanderValue.Nothing -> "Nothing"
    | WanderValue.Lambda(_, _) -> "Lambda"
    | WanderValue.NativeFunction(_) -> "NativeFunction"
    | WanderValue.Tuple(contents) -> $"[{prettyPrintTuple contents}]"

and prettyPrintTuple tuple =
    List.fold (fun res v -> $"{res}{prettyPrint v}") "" tuple