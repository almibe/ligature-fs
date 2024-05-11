// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Ligature.Wander.Environments

open Ligature.Wander
open Lib.Bool
open Lib.Bytes
open Lib.Identifier
open Lib.Ligature
open Lib.Keylime
open Lib.Int
open Lib.Wander
open Lib.DateTime

let loadWanderLibs bindings =
    match System.Environment.GetEnvironmentVariable "WANDER_LIBS" with
    | null -> failwith "WANDER_LIBS not set"
    | value ->
        
        failwith "TODO"

let bindCoreHostFunctions bindings =
    bindings
    |> Bindings.bind "Array" Lib.Array.arrayLib
    |> Bindings.bind "Assert" Lib.Assert.assertLib
    |> Bindings.bind "Bool" boolLib
    |> Bindings.bind "Bytes" bytesLib
    |> Bindings.bind "DateTime" dateTimeLib
    |> Bindings.bind "Pattern" Lib.Pattern.patternLib
    |> Bindings.bind "Keylime" keylimeLib
    |> Bindings.bind "Identifier" identifierLib
    |> Bindings.bind "Int" intLib
    |> Bindings.bind "Statement" Lib.Statement.statementLib
    |> Bindings.bind "String" Lib.String.stringLib
    |> Bindings.bind "Ulid" Lib.Ulid.ulidLib
    |> Bindings.bind "Wander" wanderLib

let standardEnvironment () =
    bindCoreHostFunctions (Bindings.newBindings ())

// let instancePrelude instance =
//     standardPrelude () |> Bindings.bind "Ligature" (ligatureLib instance)
