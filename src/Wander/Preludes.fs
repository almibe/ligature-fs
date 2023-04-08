// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Preludes

open Ligature

let notFunction = Model.WanderValue.NativeFunction (
    new Model.NativeFunction((fun args ->
        match args.Head with
        | Model.Expression.Value(Model.WanderValue.Boolean(value)) -> Ok(Model.WanderValue.Boolean(not value))
        | _ -> error "" None)))

let bindStandardLibrary bindings =
    Bindings.bind "not" notFunction bindings
