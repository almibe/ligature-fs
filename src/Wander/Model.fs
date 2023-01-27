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

type WanderValue =
| Integer of int64
| String of string
| Boolean of bool
| Identifier of Identifier
| Nothing

type Parameter = {
    name: string
    wanderType: WanderType
}

type Expression =
| LetStatement of string * Expression
| Name of string
| Scope of Expression list
| Value of WanderValue
