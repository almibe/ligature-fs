// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Bend.Lib.Statement

open Ligature.Bend.Model
open Ligature
open FsToolkit.ErrorHandling

let entityFunction = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args bindings ->
        match args with
        | [BendValue.Statement(statement)] -> Ok(BendValue.Identifier(statement.Entity))
        | _ -> error "Invalid call to Statement.entity function." None))))

let attributeFunction = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args bindings ->
        match args with
        | [BendValue.Statement(statement)] -> Ok(BendValue.Identifier(statement.Attribute))
        | _ -> error "Invalid call to Statement.attribute function." None))))

let valueFunction = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args bindings ->
        match args with
        | [BendValue.Statement(statement)] ->
            match statement.Value with
            | Value.Identifier(value) -> Ok(BendValue.Identifier(value))
            | Value.String(value) -> Ok(BendValue.String(value))
            | Value.Integer(value) -> Ok(BendValue.Int(value))
            | Value.Bytes(value) -> Ok(BendValue.Bytes(value))
        | _ -> error "Invalid call to Statement.value function." None))))

let statementLib = BendValue.Record (Map [
    ("entity", entityFunction)
    ("attribute", attributeFunction)
    ("value", valueFunction)])
