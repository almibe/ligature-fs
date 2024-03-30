// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Bend.Lib.Statement

open Ligature.Bend.Model
open Ligature
open FsToolkit.ErrorHandling

let valueFunction = BendValue.Function(Function.HostFunction (
    new HostFunction((fun args bindings ->
        match args with
        | [BendValue.Identifier(identifier)] -> BendValue.String(readIdentifier identifier)
        | _ -> error "Invalid call to Statement.value function." None))))

let statementLib = BendValue.Record (Map [
    ("value", valueFunction)
])
