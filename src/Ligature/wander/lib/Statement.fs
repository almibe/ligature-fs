// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Lib.Statement

open Ligature.Wander.Model
open Ligature.Main
open FsToolkit.ErrorHandling

let slotIdentiferToWanderValue si =
    match si with
    | PatternIdentifier.Sl slot -> WanderValue.Slot(slot)
    | PatternIdentifier.Id identifier -> WanderValue.Identifier identifier

let entityFunction =
    { Name = "entity"
      Eval =
        (fun args bindings ->
            match args with
            | [ WanderValue.Statement(statement) ] -> Ok(slotIdentiferToWanderValue statement.Entity)
            | _ -> error "Invalid call to Statement.entity function." None) }

let attributeFunction =
    { Name = "attribute"
      Eval =
        (fun args bindings ->
            match args with
            | [ WanderValue.Statement(statement) ] -> Ok(slotIdentiferToWanderValue statement.Attribute)
            | _ -> error "Invalid call to Statement.attribute function." None) }

let valueFunction =
    { Name = "value"
      Eval =
        (fun args bindings ->
            match args with
            | [ WanderValue.Statement(statement) ] ->
                match statement.Value with
                | Value.Identifier(value) -> Ok(WanderValue.Identifier(value))
                | Value.String(value) -> Ok(WanderValue.String(value))
                | Value.Int(value) -> Ok(WanderValue.Int(value))
                | Value.Bytes(value) -> Ok(WanderValue.Bytes(value))
                | Value.Slot(value) -> failwith "TODO"
            | _ -> error "Invalid call to Statement.value function." None) }

let statementLib = [ entityFunction; attributeFunction; valueFunction ]
