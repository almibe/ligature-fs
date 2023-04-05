// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Model

open Ligature

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

type WanderValue<'Body> =
    | Integer of int64
    | String of string
    | Boolean of bool
    | Identifier of Identifier
    | Nothing
    | Lambda of paramters: string list * body: 'Body list

type Parameter =
    { name: string; wanderType: WanderType }

type Case<'Condition, 'Body> = { condition: 'Condition; body: 'Body }

type Conditional<'Condition, 'Body> =
    { ifCase: Case<'Condition, 'Body>
    ; elsifCases: Case<'Condition, 'Body> list
    ; elseBody: 'Body option }

type Expression =
    | LetStatement of name: string * value: Expression
    | Name of string
    | Scope of Expression list
    | Value of WanderValue<Expression>
    | FunctionCall of name: string * arguments: Expression list
    | Conditional of Conditional<Expression, Expression>
