// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Lib

open FSharpPlus

let stdCommands =
    Commands.Core.coreCommands
    |> Map.union Commands.Assert.assertCommands
    |> Map.union Commands.Bool.boolLib
    |> Map.union Commands.Network.networkCommands
    |> Map.union Commands.TinyDL.tinyDLCommands
    |> Map.union Commands.IO.ioCommands
