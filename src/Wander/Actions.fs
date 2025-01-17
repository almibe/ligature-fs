// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Lib

open Ligature.Model
open Wander.Model
open Wander.Actions.Assert
open Wander.Actions.Core
open Wander.Actions.Network
open Wander.Actions.TinyDL

let stdActions: Actions =
    Map.ofSeq
        [ (Element "assert-equal", assertEqualAction)
          (Element "union", unionAction)
          (Element "infer", inferAction)
          (Element "clear", clearAction)
          (Element "pop", popAction)
          (Element "filter", filterAction)
          (Element "count", countAction) ]
