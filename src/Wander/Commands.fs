// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Commands

open FSharpPlus

let stdCommands =
    Wander.Lib.Core.coreCommands
    // |> Map.union Wander.Lib.Assert.assertCommands
    // |> Map.union Wander.Lib.Bool.boolLib
    // |> Map.union Wander.Lib.Network.networkCommands
    // |> Map.union Wander.Lib.TinyDL.tinyDLCommands
