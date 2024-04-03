// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Bend.Lib.Identifier

open Ligature.Bend.Model
open Ligature

let valueFunction = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args bindings ->
        match args with
        | [BendValue.Identifier(identifier)] -> Ok(BendValue.String(readIdentifier identifier))
        | _ -> error "Invalid call to Statement.value function." None))))

let toBytesFunction = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args _ ->
        match args with
        | [BendValue.Identifier(value)] -> 
            Ok(BendValue.Bytes(System.Text.Encoding.UTF8.GetBytes (readIdentifier value)))
        | _ -> error "Invalid call to map function." None))))

let fromBytesFunction = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args _ ->
        match args with
        | [BendValue.Bytes(bytes)] -> 
            match identifier (System.Text.Encoding.UTF8.GetString(bytes)) with
            | Ok(identifer) -> Ok(BendValue.Identifier(identifer))
            | Error(err) -> Error(err)
        | _ -> error "Invalid call to map function." None))))

let identifierLib = BendValue.Record (Map [
    ("toBytes", toBytesFunction)
    ("fromBytes", fromBytesFunction)
    ("value", valueFunction)])
