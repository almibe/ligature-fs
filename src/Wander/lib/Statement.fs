// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Statement

open Ligature.Wander.Model
open Ligature
open FsToolkit.ErrorHandling

let entityFunction<'t> =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(
                (fun args bindings ->
                    match args with
                    | [ WanderValue.Statement(statement) ] -> Ok(WanderValue.Identifier(statement.Entity))
                    | _ -> error "Invalid call to Statement.entity function." None)
            )
        )
    )

let attributeFunction<'t> =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(
                (fun args bindings ->
                    match args with
                    | [ WanderValue.Statement(statement) ] -> Ok(WanderValue.Identifier(statement.Attribute))
                    | _ -> error "Invalid call to Statement.attribute function." None)
            )
        )
    )

let valueFunction<'t> =
    WanderValue.Function(
        Function.HostFunction(
            new HostFunction(
                (fun args bindings ->
                    match args with
                    | [ WanderValue.Statement(statement) ] ->
                        match statement.Value with
                        | Value.Identifier(value) -> Ok(WanderValue.Identifier(value))
                        | Value.String(value) -> Ok(WanderValue.String(value))
                        | Value.Integer(value) -> Ok(WanderValue.Int(value))
                        | Value.Bytes(value) -> Ok(WanderValue.Bytes(value))
                    | _ -> error "Invalid call to Statement.value function." None)
            )
        )
    )

let statementLib<'t> =
    WanderValue.Record(
        Map
            [ ("entity", entityFunction)
              ("attribute", attributeFunction)
              ("value", valueFunction) ]
    )
