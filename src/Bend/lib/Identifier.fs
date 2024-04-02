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

let identifierLib = BendValue.Record (Map [
    ("value", valueFunction)])
