// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Lib

open FSharpPlus
open Ligature.Model
open Wander.Commands.Core

let defaultLocal = Map.ofList [ (Element "import", importCommand) ]

let stdModules =
    Map.ofList
        [ (Element "core", Commands.Core.coreCommands)
          (Element "assert", Commands.Assert.assertCommands)
          (Element "bool", Commands.Bool.boolLib)
          (Element "network", Commands.Network.networkCommands)
          (Element "tinydl", Commands.TinyDL.tinyDLCommands)
#if !FABLE_COMPILE
          (Element "io", Commands.IO.ioCommands)
#endif
          ]
