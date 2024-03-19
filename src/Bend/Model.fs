// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Model
open Identifier
open Error

type HostFunction<'T, 'Output>(eval: 'T list -> Bindings.Bindings<string, 'Output> -> Result<'Output, WanderError>) =
    member _.Run args bindings = eval args bindings

[<RequireQualifiedAccess>]
type WanderValue<'T> =
    | Int of int64
    | String of string
    | Bool of bool
    | Identifier of Identifier
    | Nothing
    | Lambda of paramters: string list * body: 'T
    | HostFunction of HostFunction<'T, WanderValue<'T>>
    | Array of WanderValue<'T> list

type Parameter =
    { name: string; tag: string }

type Case<'Condition, 'Body> = { condition: 'Condition; body: 'Body }

type Conditional<'Condition, 'Body> =
    { ifCase: Case<'Condition, 'Body>
      elsifCases: Case<'Condition, 'Body> list
      elseBody: 'Body }

[<RequireQualifiedAccess>]
type Expression =
    | Nothing
    | Int of int64
    | String of string
    | Bool of bool
    | Identifier of Identifier.Identifier
    | Let of name: string * value: Expression
    | Name of string
    | Grouping of Expression list
    | Array of Expression list
    | Application of Expression list
    | FunctionCall of name: string * arguments: Expression list
    | Conditional of Conditional<Expression, Expression>
    | Record of list<string * Expression>
    | When of list<Expression * Expression>
    | Lambda of list<string> * Expression

type WanderValue = WanderValue<Expression>
type Case = Case<Expression, Expression>
type Conditional = Conditional<Expression, Expression>
type HostFunction = HostFunction<Expression, WanderValue>
type Bindings = Bindings.Bindings<string, WanderValue>

let rec prettyPrint (value: WanderValue): string =
    match value with
    | WanderValue.Int i -> sprintf "%i" i
    | WanderValue.String s -> s
    | WanderValue.Bool b -> sprintf "%b" b
    | WanderValue.Identifier i -> $"<{(readIdentifier i)}>"
    | WanderValue.Nothing -> "Nothing"
    | WanderValue.Lambda(_, _) -> "Lambda"
    | WanderValue.HostFunction(_) -> "HostFunction"
    | WanderValue.Array(values) -> $"[{printValues values}]"

and printValues values =
    List.fold (fun x y -> x + (prettyPrint y) + ", " ) "" values
