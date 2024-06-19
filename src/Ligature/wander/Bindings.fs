// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Bindings

open Ligature.Wander
open Lib.Bool
open Lib.Bytes
open Lib.Identifier
open Lib.Ligature
open Lib.Int
open Lib.Wander
open Lib.DateTime
open Lib.Store
open Ligature.LigatureStore
open Ligature.Wander.Model

let bindCoreHostFunctions bindings store =
    bindings
    |> bindFunctions Lib.Array.arrayLib
    |> bindFunctions Lib.Assert.assertLib
    |> bindFunctions boolLib
    |> bindFunctions bytesLib
    |> bindFunctions dateTimeLib
    |> bindFunctions Lib.Ligature.ligatureLib
    |> bindFunctions identifierLib
    |> bindFunctions (storeLib store)
    |> bindFunctions intLib
    |> bindFunctions Lib.Statement.statementLib
    |> bindFunctions Lib.String.stringLib
#if !FABLE_COMPILER
    |> bindFunctions Lib.Ulid.ulidLib
#endif
    |> bindFunctions wanderLib

let coreBindings (store: LigatureStore) =
    bindCoreHostFunctions (newBindings ()) store
