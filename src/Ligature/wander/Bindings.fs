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
    // |> Bindings.bind "Array" Lib.Array.arrayLib
    |> bindFunctions Lib.Assert.assertLib
// |> Bindings.bind "Bool" boolLib
// |> Bindings.bind "Bytes" bytesLib
// |> Bindings.bind "DateTime" dateTimeLib
// |> Bindings.bind "Ligature" Lib.Ligature.ligatureLib
// |> Bindings.bind "Identifier" identifierLib
// |> Bindings.bind "Store" (storeLib store)
// |> Bindings.bind "Int" intLib
// |> Bindings.bind "Statement" Lib.Statement.statementLib
// |> Bindings.bind "String" Lib.String.stringLib
// |> Bindings.bind "Ulid" Lib.Ulid.ulidLib
// |> Bindings.bind "Wander" wanderLib

/// Provides an Environment that provides only the core Host Functions.
let coreEnvironment (store: LigatureStore) =
    bindCoreHostFunctions (newBindings ()) store

// let bindWanderLibs bindings store =
//     match System.Environment.GetEnvironmentVariable "WANDER_LIBS" with
//     | null -> failwith "WANDER_LIBS not set"
//     | value ->
//         let mutable bindings = bindings

//         System.IO.Directory.GetFiles(value, "*.wander", new System.IO.EnumerationOptions(RecurseSubdirectories = true))
//         |> Array.filter (fun file -> not <| file.Contains(".test."))
//         |> Array.iter (fun file ->
//             let scriptName = (System.IO.Path.GetFileName file).Split(".") |> Array.head
//             let script = System.IO.File.ReadAllText file

//             match Ligature.Wander.Main.run script (coreEnvironment (store)) with
//             | Ok res ->
//                 match Bindings.read scriptName bindings with
//                 | None -> bindings <- Bindings.bind scriptName res bindings
//                 | _ -> failwith $"Error {scriptName} is already bound."
//             | Error err -> failwith $"Error evaluating script {scriptName} -- {err.UserMessage}")

//         bindings

/// Provices an Environment that provides the core Host Functions and loads namespaces from WANDER_LIBS.
// let standardEnvironment (store: LigatureStore) =
//     bindCoreHostFunctions (Bindings.newBindings ()) store |> bindWanderLibs
