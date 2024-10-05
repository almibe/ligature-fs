// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Combinators

open FSharpPlus

let stdCombinators =
    Wander.Lib.Core.coreCombinators
    |> Map.union Wander.Lib.Assert.assertCombinators
    |> Map.union Wander.Lib.Bool.boolLib
    |> Map.union Wander.Lib.Network.networkCombinators
    |> Map.union Wander.Lib.TinyDL.tinyDLCombinators
